package org.mccandless.coolshit.disjointset

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * Each disjoint set is represented by a linked list.
  *
  *
  * Running times are analyzed in terms of:
  *   n: the number of makeSet operations,
  *   m: the total number of makeSet, union, and findSet operations
  *
  * Theorem:
  *   A sequence of m makeSet, union, and findSet operations, n of which are makeSet operations, takes O(m + n lg n) time.
  *
  * Proof:
  *   Each union operation unites two sets, thus we perform at most n - 1 union operations overall. We would like to bound
  *   the number of times an objects pointer back to its set object was updated. Consider a particular object x. Each time
  *   x was updated, it must have been in the smaller set being unioned. The first time x's pointer was updated, the resulting
  *   set must have had at least two members. The second time x's pointer was updated, the resulting set must have had at
  *   least 4 members. Thus, for any k <= n, after x's pointer has been updated ceil(lg k) times, the resulting set must have at least
  *   k members. The largest set has at most n members, thus each objects pointer is updated at most ceil(lg n) times
  *   throughout all union operations. The total time spent updating object pointers is O(n lg n). Recall that updating
  *   tail pointers and list lengths takes constant time per union operation. So the overall time spent in all union operations
  *   is O(n lg n). Each makeSet and findSet operation takes constant time, and there are O(m) such operations.
  *   Thus, the total time for the entire sequence is O(m + n lg n).
  *
  * Created by tdm on 11/29/18.
  */
class LinkedListDisjointSet[T] extends DisjointSetLike[T] {

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
    *
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

  val sets: mutable.Set[SetObject[T]] = mutable.Set.empty
  val addresses: mutable.Map[T, Node[T]] = mutable.Map.empty

  /**
    *
    * @param x
    * @return the [[SetObject]] containing `x`, if it exists.
    */
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
    } yield this.weightedUnion(xSet, ySet)

    sets match {
      case Some(_) => Success()
      case None => Failure(new RuntimeException)
    }
  }

  /**
    * Weighted-union heuristic for union.
    *
    * Ensures that we always append the shorter list onto the longer one.
    *
    * Without this heuristic, its possible to construct a sequence of m operations on n objects that takes
    * 𝚯(n²) time. In particular, suppose we execute n makeSet operations followed by n - 1 union operations.
    * Then m = 2n - 1. The ith union operation updates i objects, so the total number of updated objects is 𝚯(n²).
    *
    * With this heuristic, a union operation can still take 𝛀(n) time if both sets have 𝛀(n) members.
    *
    * @param x
    * @param y
    */
  protected[disjointset] def weightedUnion(x: SetObject[T], y: SetObject[T]): Unit = {
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
