package org.mccandless.coolshit.disjointset

/**
  * Uses a collection of rooted trees to represent the disjoint sets.
  *
  * Each node contains one member, and each tree represents one set. Each node points to its parent.
  * The root of each tree is the set representative, and points to itself as its parent.
  *
  * Uses two heuristics to improve on the asymptotic performance of [[LinkedListDisjointSet]]:
  *   union-by-rank is similar to the weighted union heuristic for [[LinkedListDisjointSet]]. Instead of explicitly
  *   maintaining the size of each subtree rooted at each node, we maintain a rank field which is an upper bound on the height
  *   of the node
  *
  *   path-compression is used during set representative lookup to make each node point directly to the root node.
  *
  *
  * Created by tdm on 11/29/18.
  */
class ForestDisjointSet[T] extends MutableDisjointSet[TreeNode, T] {


  override def size(): Int = this.nodes.keys.map(findRepNode).toList.distinct.length

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
    * When looking up a node, we should modify all pointers traversed to point directly to the root node.
    *
    * @param x
    * @return
    */
  override def findRepNode(x: T): Option[TreeNode[T]] = {
    this.nodes.get(x) map { node =>
      if (node != node.parent) {
        node.parent = this.findRepNode(node.parent.data).get
      }
      node.parent
    }
  }

  /**
    * Subroutine used during union.
    *
    * @param x
    * @param y
    */
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
  // upper bound on the height of the node. used in union-by-rank heuristic
  var rank: Int = 0
}