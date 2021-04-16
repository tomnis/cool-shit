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




trait MutableDisjointSet[N[T] <: NodeLike[T], T] extends DisjointSetApi[T] {




  val nodes: mutable.Map[T, N[T]] = mutable.Map.empty

  def size(): Int

  def create(x: T): Unit

  /**
    * Finds the representative object for the set containing `x`
    *
    * @param x
    * @return the representative [[NodeLike]] for the set containing `x`, if it exists.
    */
  def findRepNode(x: T): Option[N[T]]

  /**
    * Combines x and y sets
    * @param x
    * @param y
    * @return the newly created set.
    */
  def join(x: N[T], y: N[T]): Unit


  override def makeSet(x: T): Try[Unit] = {
    if (this.findRepNode(x).isDefined) Failure(new RuntimeException(s"already have a set containing $x"))
    else Try(this create x)
  }

  override def union(x: T, y: T): Try[Unit] = {
    val r = for {
      xSet: N[T] <- this findRepNode x
      ySet: N[T] <- this findRepNode y
    } yield this.join(xSet, ySet)

    r match {
      case Some(_) => Success()
      case None => Failure(new RuntimeException)
    }
  }

  override def findSet(x: T): Option[T] = this.findRepNode(x) map { _.data }
}

trait NodeLike[T] {
  val data: T
}