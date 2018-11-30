package org.mccandless.coolshit.disjointset

import scala.collection.mutable
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/**
  * Some common operations needed by our particular implementations of disjoint sets.
  *
  *
  * Operations are analyzed in terms of `n`, the number of calls to `makeSet`, and `m`, the total number of operations.
  *
  * Created by tdm on 11/29/18.
  */
// TODO rename to DisjointSetPointer?
// or DisjointSetCommon?
trait MutableDisjointSet[T] extends DisjointSetApi[T] {

  /**
    * Data type used as a wrapper for the representative object.
    * - SetObject (or sentinel) for linkedlist
    * - Node for forest
    *
    * @tparam _
    */
  type M[_]
  type N[_]

  val sets: mutable.Set[M[T]] = mutable.Set.empty
  val nodes: mutable.Map[T, N[T]] = mutable.Map.empty

  def create(x: T): Unit

  /**
    *
    * @param x
    * @return the [[NodeLike]] containing `x`, if it exists.
    */
  def findNode(x: T): Option[N[T]]


  /**
    * Combines x and y sets
    * @param x
    * @param y
    * @return the newly created set.
    */
  def join(x: N[T], y: N[T]): Unit


  override def makeSet(x: T): Try[Unit] = {
    if (this.nodes contains x) Failure(new RuntimeException(s"already have a set containing $x"))
    else Try(this create x)
  }

  override def union(x: T, y: T): Try[Unit] = {
    val r = for {
      xSet: N[T] <- this findNode x
      ySet: N[T] <- this findNode y
    } yield this.join(xSet, ySet)

    r match {
      case Some(_) => Success()
      case None => Failure(new RuntimeException)
    }
  }
}

//trait NodeLike[T] {
//  val data: T
//}