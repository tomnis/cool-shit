package org.mccandless.coolshit.zipper

import org.mccandless.coolshit.BaseSpec

/**
  * Created by tomas.mccandless on 2019-03-20.
  */
class ZipperSpec extends BaseSpec{

  "zipper" should "zip" in {
    // how can we modify this?
    val tree = Branch(
      left = Branch(
        left = Branch(Leaf(1), Leaf(2)),
        right = Branch(Leaf(3), Leaf(4))
      ),
      right = Branch(
        left = Branch(Leaf(5), Leaf(6)),
        right = Branch(Leaf(7), Leaf(8))
      )
    )

    // all this .copy seems ugly
//    val tPrime = t.copy(left = t.left.copy(right = Leaf(3)))


    // create a zipper, this uses context Top to signify root of tree
    val z: Zipper[Int] = Zipper(tree)
    // simple sanity check
    z.left.left.right.focus should be (Leaf(2))

    // traverse and modify. common subtrees unmodified are shared in memory, similar to copy on write
    // in general, zipper allows for elements close to the cursor to be efficiently updated
    val t2: Tree[Int] = z.left.left.right.modify(_ => Leaf(45)).upmost.focus
    // we could also graft in an entirely different subtree here
//    val t2 = z.left.left.right.modify(_ => Branch(Leaf(1), Leaf(2))).upmost.focus

    // "reify" the tree and check that our update took
    Zipper(t2).left.left.right.focus should be (Leaf(45))
    println(t2)

    // should be able to recover the original tree unmodified
    z.left.left.upmost.focus should be (tree)
  }
}
