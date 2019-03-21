package org.mccandless.coolshit.zipper

import scala.math.max
import scala.annotation.tailrec

/**
  * Created by tomas.mccandless on 2019-03-20.
  */
sealed trait Tree[+A] {

  /** @return number of nodes in this tree. */
  def size: Int = {
    @tailrec def go(acc: Int, queue: List[Tree[A]]): Int = {
      queue match {
        case Nil => acc
        case Leaf(_) :: xs => go(acc + 1, xs)
        case Branch(left, right) :: xs => go(acc + 1, left :: right :: xs)
      }
    }

    go(0, List(this))
  }


  /** @return max length of path to leaf. */
  def depth: Int = {
    this match {
      case Leaf(_) => 0
      case Branch(left, right) => max(left.depth, right.depth)
    }
  }


  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(this.map(left)(f), this.map(right)(f))
    }
  }

  // 3.29
  def fold[A, B](tree: Tree[A], zero: B)(f: (B, A) => B)(g: (B, B) => B): B = {

    @tailrec def go(acc: B, queue: List[Tree[A]]): B = {
      queue match {
        case Nil => acc
        case Leaf(a) :: trees => go(f(acc, a), trees)
        case Branch(left, right) :: trees => go(g(acc, zero), left :: right :: trees)
      }
    }

    go(zero, List(tree))
  }


  def foldTailRec[A, B](tree: Tree[A], zero: B)(f: (B, A) => B)(g: (B, B) => B): B = {

    @tailrec def go(acc: B, queue: List[Tree[A]]): B = {
      queue match {
        case Nil => acc
        case Leaf(a) :: trees => go(f(acc, a), trees)
        case Branch(left, right) :: trees => go(g(acc, zero), left :: right :: trees)
      }
    }

    go(zero, List(tree))
  }



}



case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
