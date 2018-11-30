package org.mccandless.coolshit.disjointset

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * Each disjoint set is represented by a linked list.
  *
  * Created by tdm on 11/29/18.
  */
class LinkedListDisjointSet[T] extends DisjointSetLike[T] {

  val sets: mutable.Set[SetObject[T]] = mutable.Set.empty
  val addresses: mutable.Map[T, Node[T]] = mutable.Map.empty

  protected[disjointset] def findSetObject(x: T): Option[SetObject[T]] = this.addresses get x map { _.set }

  /**
    * Creates a new linked list whose only object is `x`
    *
    * Runs in O(1) time.
    *
    * @param x
    */
  override def makeSet(x: T): Try[Unit] = {

    this.findSetObject(x) match {
      case Some(_) =>
        Failure(new RuntimeException(s"already have a set containing $x"))
      case None =>
        val s: SetObject[T] = SetObject.empty
        val n: Node[T] = Node(x)(s)
        s.head = n
        s.tail = n
        s.size = 1
        this.sets += s
        this.addresses += (x -> n)
        Success()
    }
  }

  /**
    * Unions the two sets together by appending lists.
    *
    * Destroy's the set object for `y`, and updates all pointers in new list to point to the correct sentinel.
    *
    * @param x
    * @param y
    */
  override def union(x: T, y: T): Try[Unit] = {
    val sets = for {
      xSet <- this findSetObject x
      ySet <- this findSetObject y
    } yield this.union(xSet, ySet)

    sets match {
      case Some(_) => Success()
      case None => Failure(new RuntimeException)
    }
  }

  /**
    * Weighted-union heuristic for union.
    *
    * Ensures that we always append the shorter list onto the longer one.
    * @param x
    * @param y
    */
  protected[disjointset] def union(x: SetObject[T], y: SetObject[T]): Unit = {
    val (shorter, longer) = if (x.size < y.size) (x, y) else (y, x)

    // delete the old shorter list
    // its important we do this before modifying shorter
    this.sets -= shorter

    // update the shorters lists set pointers
    shorter.head.setSet(longer)

    // append the shorter list onto the longer one. update the tail pointers
    longer.tail.maybeNext = Option(shorter.head)
    longer.tail = shorter.tail
    longer.size += shorter.size
  }


  /**
    * Looks up the representative of the set containing `x`.
    *
    * Runs in O(1) time.
    *
    * @param x
    * @return a pointer to the representative of the unique set containing `x`.
    */
  override def findSet(x: T): Option[T] = this findSetObject x map { _.head.value }
}


/**
  * Maintains `head` pointing to first element in a list, and `tail`, pointing to the last element.
  *
  * Set representative is the first element in the list.
  *
  * @param head
  * @param tail
  * @tparam T
  */
case class SetObject[T](var head: Node[T], var tail: Node[T], var size: Int)
object SetObject {
  def empty[T]: SetObject[T] = new SetObject[T](null, null, size = 0)
}


/**
  * Node in a linked list.
  *
  * Each node maintains a pointer to the set sentinel object it is a member of.
  * Note that `set` is defined in a second parameter list so that it is not considered eligible for fields taking part in
  * hashCode. This avoids a circular reference when computing SetObject.hashCode
  *
  * @param value
  * @param maybeNext
  * @param set a pointer to the "sentinel" set object.
  * @tparam T
  */
case class Node[T](value: T, var maybeNext: Option[Node[T]] = None)(var set: SetObject[T]) {
  def setSet(sPrime: SetObject[T]): Unit = {
    this.set = sPrime
    this.maybeNext foreach { _.setSet(sPrime) }
  }
}
