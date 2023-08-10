package kvstore

import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout, actorRef2Scala}

import scala.concurrent.duration.*

object Replicator:
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(Replicator(replica))

class Replicator(val replica: ActorRef) extends Actor:
  import Replicator.*
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  val TIMEOUT = 10.millis
  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq() =
    val ret = _seqCounter
    _seqCounter += 1
    ret

  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = normal

  val normal: Receive =
    case msg@Replicate(key, valueOption, id) =>
      val seq = nextSeq()
      acks += seq -> (sender(), msg)
      val snapshot = Snapshot(key, valueOption, seq)
      //println(f"replicator ${self}, sending ${snapshot}")
      pending = pending.appended(snapshot)
      context.become(waiting)
      replica ! snapshot
      context.setReceiveTimeout(TIMEOUT)

  val waiting: Receive =
    case SnapshotAck(key, seq) =>
      //println(f"replicator ${self}, received ${SnapshotAck(key, seq)}")
      if (pending.nonEmpty) {
        val curWaiting = pending.head
        if (seq == curWaiting.seq) {
          pending = pending.tail
          acks(seq)._1 ! Replicated(acks(seq)._2.key, acks(seq)._2.id)
          acks = acks.removed(seq)
        }
        if (pending.nonEmpty) {
          replica ! pending.head
        } else {
          context.setReceiveTimeout(Duration.Undefined)
          context.become(normal)
        }
      } else {
        context.become(normal)
      }
    case msg@Replicate(key, valueOption, id) =>
      val seq = nextSeq()
      acks += seq -> (sender(), msg)
      pending = pending.appended(Snapshot(key, valueOption, seq))
    case ReceiveTimeout =>
      //println(f"replicator ${self}, sending ${pending.head}")
      replica ! pending.head
