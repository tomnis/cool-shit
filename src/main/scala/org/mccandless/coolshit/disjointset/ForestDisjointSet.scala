package org.mccandless.coolshit.disjointset

/**
  * Uses a collection of rooted trees to represent the disjoint sets.
  *
  * Each node contains one member, and each tree represents one set. Each node points to its parent.
  * The root of each tree is the set representative, and points to itself as its parent.
  *
  * Created by tdm on 11/29/18.
  */
class ForestDisjointSet[T] extends MutableDisjointSet[TreeNode, T] {


  protected[disjointset] def size(): Int = this.nodes.keys.map(findRepNode).toList.distinct.length
  /**
    * Creates a new set containing a single element `x`.
    *
    * Assumes that `x` is not contained in any existing set.
    *
    * @param x
    */
  override def create(x: T): Unit = this.nodes += (x -> TreeNode(x))


  /**
    * Implements path compression heuristic.
    *
    * @param x
    * @return
    */
  override def findRepNode(x: T): Option[TreeNode[T]] = {
    for {
      s <- this.nodes get x
      sp <- this.findRepNode(s.parent.data) if s.parent != s
      _ = s.parent = sp
    } yield sp
  }

  override def join(x: TreeNode[T], y: TreeNode[T]): Unit = {
    if (x.rank > y.rank) {
      y.parent = x
    }
    else {
      x.parent = y
      if (x.rank == y.rank) {
        y.rank += 1
      }
    }
  }
}

case class TreeNode[T](data: T) extends NodeLike[T] {
  // a root node has itself as a parent
  var parent: TreeNode[T] = this
  var rank: Int = 0
}