package org.mccandless.coolshit.disjointset

import scala.util.Try

/**
  * Maintains a collection of disjoint sets.
  *
  * Each set is identified by a representative element.
  *
  * Operations are anlyzed in terms of `n`, the number of calls to `makeSet`, and `m`, the total number of operations.
  *
  * Created by tdm on 11/29/18.
  */
trait DisjointSetLike[T] {

  /**
    * Creates a new set whose only member is `x`.
    *
    * Note that we require that `x` is not already a member of any other set.
    * @param x
    */
  def makeSet(x: T): Try[Unit]


  /**
    * Unites the sets that contain `x` and `y`.
    *
    * Requires that the two sets are already disjoint prior to the operation.
    *
    * @param x
    * @param y
    */
  def union(x: T, y: T): Try[Unit]


  /**
    *
    * @param x
    * @return a pointer to the representative of the unique set containing `x`.
    */
  def findSet(x: T): Option[T]
}
