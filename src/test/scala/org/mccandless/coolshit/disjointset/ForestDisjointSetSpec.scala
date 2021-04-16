package org.mccandless.coolshit.disjointset

import org.mccandless.coolshit.BaseSpec

/**
  *
  * Created by tdm on 11/30/18.
  */
class ForestDisjointSetSpec extends BaseSpec {

  "ForestDisjointSet" should "make set" in {
    val s = new ForestDisjointSet[Int]

    s.makeSet(4)
    s should have size 1
  }


  it should "not allow duplicates" in {
    val s = new ForestDisjointSet[Int]

    s.makeSet(4)
    s should have size 1
    s.makeSet(4).isFailure should be (true)
  }


  it should "find" in {
    val s = new ForestDisjointSet[Int]
    s should have size 0

    s.findSet(1) should be (None)
    s.makeSet(1)
    s.findSet(1) should be (Some(1))
  }


  it should "union" in {
    val s = new ForestDisjointSet[Int]

    s.makeSet(4)
    s.makeSet(5)
    s.makeSet(6)
    s should have size 3

    s.union(4, 5)
    s should have size 2

    s.union(5, 6)
    s should have size 1

    s.findSet(6) should be (Some(5))
    s.findSet(5) should be (Some(5))


    def recur(i: Int): Unit = {
      println(i)
      recur(i + 1)
    }

    recur(0)
  }


  def recur(i: Int): Unit = {
    println(i)
    recur(i + 1)
  }

  recur(0)
}
