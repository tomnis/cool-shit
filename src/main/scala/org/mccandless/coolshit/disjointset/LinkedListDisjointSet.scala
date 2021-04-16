package org.mccandless.coolshit.disjointset

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
class LinkedListDisjointSet[T] extends MutableDisjointSet[ListNode, T] {



  /**
    * @return the number of disjoint sets we are tracking.
    */
  def size(): Int = this.nodes.values.map(_.sentinel).toList.distinct.length

  /**
    * Creates a new set containing a single element `x`.
    *
    * Assumes that `x` is not contained in any existing set.
    *
    * @param x
    */
  override def create(x: T): Unit = {
    val s: Sentinel[T] = Sentinel.empty
    val n: ListNode[T] = ListNode(x)(s)
    s.head = n
    s.tail = n
    s.size = 1
    this.nodes += (x -> n)
  }

  /**
    * @param x
    * @return the representative [[NodeLike]] for the set containing `x`, if it exists.
    */
  override def findRepNode(x: T): Option[ListNode[T]] = this.nodes get x map { _.sentinel.head }

  /**
    * Joins two sets together to make a new set.
    *
    * Used in union.
    *
    * @param x
    * @param y
    */
  override def join(x: ListNode[T], y: ListNode[T]): Unit = this.weightedUnion(x.sentinel, y.sentinel)

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
  protected[disjointset] def weightedUnion(x: Sentinel[T], y: Sentinel[T]): Unit = {
    val (shorter, longer) = if (x.size < y.size) (x, y) else (y, x)

    // update the shorters lists set pointers
    shorter.head.setSentinel(longer)

    // append the shorter list onto the longer one. update the tail pointers
    longer.tail.maybeNext = Option(shorter.head)
    longer.tail = shorter.tail
    longer.size += shorter.size
  }
}

case class Sentinel[T](var head: ListNode[T], var tail: ListNode[T], var size: Int)
object Sentinel {
  def empty[T]: Sentinel[T] = new Sentinel[T](null, null, size = 0)
}


/**
  * Node in a linked list.
  *
  * Each node maintains a pointer to the set sentinel object it is a member of.
  *
  * Note that `set` is defined in a second parameter list so that it is not considered eligible for fields taking part in
  * hashCode. This avoids a circular reference when computing SetObject.hashCode
  *
  * @param data
  * @param maybeNext
  * @param sentinel a pointer to the "sentinel" set object.
  * @tparam T
  */
case class ListNode[T](data: T, var maybeNext: Option[ListNode[T]] = None)(var sentinel: Sentinel[T]) extends NodeLike[T] {
  // TODO use setter syntax
  def setSentinel(sPrime: Sentinel[T]): Unit = {
    this.sentinel = sPrime
    this.maybeNext foreach { _.setSentinel(sPrime) }
  }
}
