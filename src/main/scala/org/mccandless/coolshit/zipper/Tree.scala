package org.mccandless.coolshit.zipper

/**
  * Created by tomas.mccandless on 2019-03-20.
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]