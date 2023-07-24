/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor.*
import scala.collection.immutable.Queue

object BinaryTreeSet:

  trait Operation:
    def requester: ActorRef
    def id: Int
    def elem: Int

  trait OperationReply:
    def id: Int

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply



class BinaryTreeSet extends Actor:
  import BinaryTreeSet.*
  import BinaryTreeNode.*

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional (used to stash incoming operations during garbage collection)
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case GC => {
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      //println("GC started!")
      root ! CopyTo(newRoot)
    }
    case msg: Operation => {
      //println("Normal received " + msg)
      root ! msg
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case msg: Operation => pendingQueue = pendingQueue.appended(msg)
    case CopyFinished => {
      //println("GC done! Stopping old root")
      val oldRoot = root
      root = newRoot
      val pendingMsg = pendingQueue
      pendingQueue = Queue.empty[Operation]
      context.stop(oldRoot)
      context.become(normal)
      pendingMsg.foreach(
        msg => {
          //println("Sending msg " + msg)
          root ! msg
        }
      )
    }
    case GC =>
  }


object BinaryTreeNode:
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  /**
   * Acknowledges that a copy has been completed. This message should be sent
   * from a node to its parent, when this node and all its children nodes have
   * finished being copied.
   */
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor:
  import BinaryTreeNode.*
  import BinaryTreeSet.*

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case msg@Contains(requester, id, target) =>
      if (elem == target && !removed) {
        requester ! ContainsResult(id, true)
      } else if (target > elem && subtrees.contains(Right)) {
        subtrees(Right) ! msg
      } else if (target < elem && subtrees.contains(Left)) {
        subtrees(Left) ! msg
      } else {
        requester ! ContainsResult(id, false)
      }
    case msg@Insert(requester, id, target) =>
      if (elem == target) {
        removed = false
        requester ! OperationFinished(id)
      } else if (target > elem) {
        if (subtrees.contains(Right))
          subtrees(Right) ! msg
        else {
          subtrees = subtrees ++ Map(Right -> context.actorOf(props(target, initiallyRemoved = false)))
          requester ! OperationFinished(id)
        }
      } else { //  target < elem)
        if (subtrees.contains(Left))
          subtrees(Left) ! msg
        else {
          subtrees = subtrees ++ Map(Left -> context.actorOf(props(target, initiallyRemoved = false)))
          requester ! OperationFinished(id)
        }
      }
    case msg@Remove(requester, id, target) =>
      if (elem == target) {
        removed = true
        requester ! OperationFinished(id)
      } else if (target > elem) {
        if (subtrees.contains(Right))
          subtrees(Right) ! msg
        else {
          requester ! OperationFinished(id)
        }
      } else { //  target < elem)
        if (subtrees.contains(Left))
          subtrees(Left) ! msg
        else {
          requester ! OperationFinished(id)
        }
      }
    case msg@CopyTo(treeNode) => {
      val actors = subtrees.values.toSet + sender()
      //println(f"CopyTo started [${toString}] ${elem}/${removed} with copying(${actors.size}, ${removed})")
      context.become(copying(actors, removed))
      subtrees.values.foreach(c => c ! msg)
      if (!removed)
        treeNode ! Insert(self, elem, elem)
      else if (subtrees.isEmpty) {
        //println("CopyTo finished")
        context.become(normal)
        sender() ! CopyFinished
      }
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case _: OperationFinished =>
      //println(f"CopyTo ${elem} OperationFinished ${expected.size}")
      if (expected.size == 1) {
        //println("CopyTo finished")
        context.become(normal)
        expected.toList.head ! CopyFinished
      } else {
        context.become(copying(expected, true))
      }
    case CopyFinished => {
      val updatedExpected = expected - sender()
      //println(f"CopyTo ${elem} CopyFinished ${expected.size}/${updatedExpected.size}")
      if (updatedExpected.size == 1 && insertConfirmed) {
        //println("CopyTo finished")
        context.become(normal)
        updatedExpected.toList.head ! CopyFinished
      } else {
        context.become(copying(updatedExpected, insertConfirmed))
      }
    }
  }


