package org.mccandless.coolshit.disjointset

import scala.util.Try

/**
  * Defines the API of a Union-Find data structure for maintaining a collection of disjoint sets.
  *
  * Created by tdm on 11/29/18.
  */
trait DisjointSetApi[T] {

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
