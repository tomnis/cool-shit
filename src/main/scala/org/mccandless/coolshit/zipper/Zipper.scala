package org.mccandless.coolshit.zipper

/**
  *
  * data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a)
  *
  * Represents path from subtree to the root (rather than the other way round)
  *
  * focus of a subtree together with a context
  * Created by tomas.mccandless on 2019-03-20.
  */
sealed trait Context[+A]

// root of the tree
case object Top extends Context[Nothing]
// left part of a branch of which the right part was t and whose parent had context c
case class Left[A](cxt: Context[A], tree: Tree[A]) extends Context[A]
case class Right[A](tree: Tree[A], cxt: Context[A]) extends Context[A]


case class Zipper[A](focus: Tree[A], context: Context[A]) {

  // (Fork l r, c) = (l, L c r)
  def left: Zipper[A] = this match {
    case Zipper(Branch(left, right), cxt) => Zipper(left, Left(cxt, right))
    case _ => ???
  }


  // (Fork l r, c) = (r, R l c)
  def right: Zipper[A] = this match {
    case Zipper(Branch(left, right), cxt) => Zipper(right, Right(left, cxt))
    case _ => ???
  }


  //up (t, L c r) = (Fork t r, c)
  //up (t, R l c) = (Fork l t, c)
  def up: Zipper[A] = this match {
    case Zipper(focus, Left(cxt, right)) => Zipper(Branch(focus, right), cxt)
    case Zipper(focus, Right(left, cxt)) => Zipper(Branch(left, focus), cxt)
    case _ => ???
  }


  // upmost :: Loc a -> Loc a
  // upmost l@(t, Top) = l
  // upmost l = upmost (up l)
  def upmost: Zipper[A] = this match {
    case Zipper(_, Top) => this
    case z => z.up.upmost
  }


  // modify :: Loc a -> (Tree a -> Tree a) -> Loc a
  // modify (t, c) f = (f t, c)
  def modify(f: Tree[A] => Tree[A]): Zipper[A] = {
    this.copy(focus = f(this.focus))
  }
}


object Zipper {

  def apply[A](t: Tree[A]): Zipper[A] = Zipper(t, Top)
}


