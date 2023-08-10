package kvstore

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorRef, OneForOneStrategy, PoisonPill, Props, ReceiveTimeout, SupervisorStrategy, Terminated, actorRef2Scala}
import kvstore.Arbiter.*
import akka.pattern.{ask, pipe}

import scala.concurrent.duration.*
import akka.util.Timeout

object Replica:
  sealed trait Operation:
    def key: String
    def id: Long
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(Replica(arbiter, persistenceProps))

  val TIMEOUT = 10.millis

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor:
  import Replica.*
  import Replicator.*
  import Persistence.*
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  var expectedSeq = 0

  val persister = context.actorOf(persistenceProps)
  var persistQueue = Vector.empty[Persist]
  var persistAcks = Map.empty[Long, ActorRef]

  var leaderPersistQueue = Vector.empty[(Persist, Long)]
  var replicaPendingSnapshot = Vector.empty[Snapshot]
  var leaderPendingOperation = Vector.empty[Operation]

  var pendingJoiners = Set.empty[ActorRef]
  var pendingJoinersReplicates = Vector.empty[Replicate]

  arbiter ! Join

  override val supervisorStrategy = OneForOneStrategy() {
    case _: PersistenceException => Restart
  }

  def receive =
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)

  /* TODO Behavior for  the leader role. */
  val leader: Receive =
    case Insert(key, value, id) =>
      val persist = Persist(key, Some(value), id)
      //println(f"leader ${self} received ${Insert(key, value, id)} and sent ${persist}")
      persistAcks += (id -> sender())
      leaderPersistQueue = leaderPersistQueue.appended((persist, System.currentTimeMillis()))
      context.become(leaderWaitPersistence(false, replicators))
      sendPersistAndReplication(persist)
      context.setReceiveTimeout(TIMEOUT)
    case Remove(key, id) =>
      val persist = Persist(key, None, id)
      //println(f"leader ${self} received ${Remove(key, id)} and sent ${persist}")
      persistAcks += (id -> sender())
      leaderPersistQueue = leaderPersistQueue.appended((persist, System.currentTimeMillis()))
      context.become(leaderWaitPersistence(false, replicators))
      sendPersistAndReplication(persist)
      context.setReceiveTimeout(TIMEOUT)
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
    case Replicas(replicas) =>
      val (newReplicas, _, _) = getUpdatedReplicas(replicas)
      pendingJoiners = pendingJoiners ++ newReplicas
      switchToNormalLeader()

  private def getUpdatedReplicas(replicas: Set[ActorRef]): (Set[ActorRef], Set[ActorRef], Set[ActorRef]) = {
    val added = replicas
      .filter(r => !secondaries.contains(r) && r != self)
    val removed = secondaries
      .filter(kv => !replicas.contains(kv._1))
      .keySet
    val removedReplicators = removed.map(r => secondaries(r))
    if (removed.nonEmpty) {
      removedReplicators.foreach(r => context.stop(r))
      replicators = replicators -- removedReplicators
      secondaries = secondaries.filter(s => removed.contains(s._1))
    }
    (added, removed, removedReplicators)
  }


  private def switchToNormalLeader(): Unit = {
    val newReplicas = pendingJoiners
    if (newReplicas.nonEmpty) {
      secondaries = secondaries ++ newReplicas.map(r => r -> context.actorOf(Replicator.props(r))).toMap
      replicators = replicators ++ newReplicas.map(r => secondaries(r))

      pendingJoiners = Set.empty[ActorRef]
    }
    if (newReplicas.nonEmpty && kv.nonEmpty) {
      val newReplicators = newReplicas.map(r => secondaries(r))
      pendingJoinersReplicates = kv
        .zipWithIndex
        .map((kv, i) => Replicate(kv._1, Some(kv._2), i))
        .toVector
      context.setReceiveTimeout(Duration.Undefined)
      context.become(leaderSyncNewJoiner(newReplicators, newReplicators))
      newReplicators.foreach(r => r ! pendingJoinersReplicates.head)
    } else if (leaderPendingOperation.nonEmpty) {
      val nextOp = leaderPendingOperation.head
      leaderPendingOperation = leaderPendingOperation.tail
      val persist = nextOp match {
        case Insert(key, value, id) =>
          Persist(key, Some(value), id)
        case Remove(key, id) =>
          Persist(key, None, id)
      }
      leaderPersistQueue = leaderPersistQueue.appended((persist, System.currentTimeMillis()))
      context.become(leaderWaitPersistence(false, replicators))
      sendPersistAndReplication(persist)
      context.setReceiveTimeout(TIMEOUT)
    } else {
      context.setReceiveTimeout(Duration.Undefined)
      context.become(leader)
    }
  }

  def continueSyncNewJoiner(allReplicators: Set[ActorRef], updatedWait: Set[ActorRef]): Unit = {
    if (updatedWait.isEmpty) {
      pendingJoinersReplicates = pendingJoinersReplicates.tail
      if (pendingJoinersReplicates.isEmpty) {
        switchToNormalLeader()
      } else {
        allReplicators.foreach(r => r ! pendingJoinersReplicates.head)
        context.become(leaderSyncNewJoiner(allReplicators, allReplicators))
      }
    } else {
      context.become(leaderSyncNewJoiner(allReplicators, updatedWait))
    }
  }
  def leaderSyncNewJoiner(allReplicators: Set[ActorRef], waitReplicators: Set[ActorRef]): Receive = {
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
    case msg@Insert(key, value, id) =>
      persistAcks += (id -> sender())
      leaderPendingOperation = leaderPendingOperation.appended(msg)
      //println(f"leader ${self} received ${msg} while syncing ${persistAcks}, ${leaderPendingOperation}")
    case msg@Remove(key, id) =>
      persistAcks += (id -> sender())
      leaderPendingOperation = leaderPendingOperation.appended(msg)
      //println(f"leader ${self} received ${msg} while syncing ${persistAcks}, ${leaderPendingOperation}")
    case _: Replicated =>
      val updatedWait = waitReplicators - sender()
      continueSyncNewJoiner(allReplicators, updatedWait)
    case Replicas(replicas) =>
      val (newReplicas, _, removedReplicators) = getUpdatedReplicas(replicas)
      pendingJoiners = pendingJoiners ++ newReplicas
      val updatedAllReplicators = allReplicators.diff(removedReplicators)
      val updatedWaitReplicators = waitReplicators.diff(removedReplicators)
      continueSyncNewJoiner(updatedAllReplicators, updatedWaitReplicators)
    case ReceiveTimeout => println("Timeout during leaderSyncNewJoiner")
  }
  private def sendPersistAndReplication(persist: Persist): Unit = {
    persister ! persist
    replicators.foreach(
      rep => rep ! Replicate(persist.key, persist.valueOption, persist.id)
    )
  }

  private def ackLeaderPersist(success: Boolean): Unit = {
    val acked = leaderPersistQueue.head
    leaderPersistQueue = leaderPersistQueue.tail
    if (success) {
      acked._1.valueOption match {
        case None => kv = kv.removed(acked._1.key)
        case Some(value) => kv = kv + (acked._1.key -> value)
      }
      persistAcks(acked._1.id) ! OperationAck(acked._1.id)
    } else {
      persistAcks(acked._1.id) ! OperationFailed(acked._1.id)
    }
    persistAcks = persistAcks.removed(acked._1.id)
    switchToNormalLeader()
  }
  def leaderWaitPersistence(persisted: Boolean, waitReplicators: Set[ActorRef]): Receive = {
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
    case msg@Insert(key, value, id) =>
      //println(f"leader ${self} received ${msg} while persisting ${persisted},${waitReplicators.size}")
      persistAcks += (id -> sender())
      leaderPendingOperation = leaderPendingOperation.appended(msg)
    case msg@Remove(key, id) =>
      //println(f"leader ${self} received ${msg} while persisting ${persisted},${waitReplicators.size}")
      persistAcks += (id -> sender())
      leaderPendingOperation = leaderPendingOperation.appended(msg)
    case ReceiveTimeout =>
      val cur = System.currentTimeMillis()
      if (cur - leaderPersistQueue.head._2 >= 1000) {
        //println(f"xxxxxxxxxxxx leader ${self} timeout ${cur} failed, ${persisted},${waitReplicators.size} queue ${leaderPersistQueue}")
        ackLeaderPersist(false)
      } else if (!persisted) {
        //println(f"xxxxxxxxxxxx leader ${self} timeout ${cur} waiting, ${persisted},${waitReplicators.size} queue ${leaderPersistQueue}, sent ${leaderPersistQueue.head._1}")
        persister ! leaderPersistQueue.head._1
      }
    case Persisted(key, id) =>
      //println(f"leader ${self}, before received ${Persisted(key, id)}, ${persisted},${waitReplicators.size} ${leaderPersistQueue} ${persistAcks} ${leaderPendingOperation}")
      if (leaderPersistQueue.head._1.id == id) {
        if (waitReplicators.isEmpty) {
          ackLeaderPersist(true)
        } else {
          context.become(leaderWaitPersistence(true, waitReplicators))
        }
      } else {
        println(f"leader ignoring persist with id ${id}")
      }
      //println(f"leader ${self}, after received ${Persisted(key, id)}, ${persisted},${waitReplicators.size} ${leaderPersistQueue} ${persistAcks} ${leaderPendingOperation}")
    case Replicated(key, id) =>
      val updatedWaitReplicators = waitReplicators - sender()
      if (persisted && updatedWaitReplicators.isEmpty) {
        ackLeaderPersist(true)
      } else {
        context.become(leaderWaitPersistence(persisted, updatedWaitReplicators))
      }
    case Replicas(replicas) =>
      val (newReplicas, _, removedReplicators) = getUpdatedReplicas(replicas)
      pendingJoiners = pendingJoiners ++ newReplicas
      val updatedWaitReplicators = waitReplicators.diff(removedReplicators)
      if (persisted && updatedWaitReplicators.isEmpty) {
        ackLeaderPersist(true)
      } else {
        context.become(leaderWaitPersistence(persisted, updatedWaitReplicators))
      }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive =
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
    case Snapshot(key, valueOption, seq) =>
      if (seq == expectedSeq) {
        valueOption match {
          case Some(value) => kv = kv + (key -> value)
          case _ => kv = kv.removed(key)
        }
        val persist = Persist(key, valueOption, seq)
        //println(f"replica ${self}, sending ${persist}")
        persistAcks += (seq -> sender())
        persistQueue = persistQueue.appended(persist)
        persister ! persist
        context.become(replicaWaitPersistence)
        context.setReceiveTimeout(TIMEOUT)
      } else if (seq < expectedSeq) {
        //println(f"replica ${self}, sending ${SnapshotAck(key, seq)}")
        sender() ! SnapshotAck(key, seq)
      } else {
        //println(f"replica ${self}, ignored ${seq} given ${expectedSeq}")
      }

  val replicaWaitPersistence: Receive =
    case Get(key, id) =>
      sender() ! GetResult(key, kv.get(key), id)
    case Persisted(key, seq) =>
      //println(f"replica ${self}, received ${Persisted(key, seq)}")
      persistQueue = persistQueue.tail
      persistAcks(seq) ! SnapshotAck(key, seq)
      persistAcks = persistAcks.removed(seq)
      expectedSeq += 1
      if (replicaPendingSnapshot.nonEmpty) {
        val nextMsg = replicaPendingSnapshot.head
        nextMsg.valueOption match {
          case Some(value) => kv = kv + (key -> value)
          case _ => kv = kv.removed(key)
        }
        replicaPendingSnapshot = replicaPendingSnapshot.tail
        val persist = Persist(nextMsg.key, nextMsg.valueOption, nextMsg.seq)
        persistQueue = persistQueue.appended(persist)
        persister ! persist
      } else {
        context.setReceiveTimeout(Duration.Undefined)
        context.become(replica)
      }
    case ReceiveTimeout =>
      //println(f"replica ${self} timeout, sending ${persistQueue.head}")
      persister ! persistQueue.head
    case msg@Snapshot(key, valueOption, seq) =>
      if (!persistAcks.contains(seq)) {
        replicaPendingSnapshot = replicaPendingSnapshot.appended(msg)
        persistAcks += (seq -> sender())
      } else {
        //println(f"replica ${self}, snapshot ${msg} already in progress")
      }



