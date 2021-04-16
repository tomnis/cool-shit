package example

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tomas.mccandless on 12/12/18.
  */
class Day12Spec extends FlatSpec with Matchers {


  "day 12" should "day 12" in {


    var state: String = "...#..#.#..##......###...###"

    val rules: Map[String, String] = Map(
      "...##" -> "#",
      "..#.." -> "#",
      ".#..." -> "#",
      ".#.#." -> "#",
      ".#.##" -> "#",
      ".##.." -> "#",
      ".####" -> "#",
      "#.#.#" -> "#",
      "#.###" -> "#",
      "##.#." -> "#",
      "##.##" -> "#",
      "###.." -> "#",
      "###.#" -> "#",
      "####." -> "#"
    )

    def nextSubState(subState: String): String = rules.getOrElse(subState, ".")

    0 to 20 foreach { i =>
      println(s"$i: $state")

      val subStrings: List[String] = (".." + state + "..").sliding(5).toList
      val news: List[String] = subStrings.map(nextSubState)
      val newss: String = news.mkString("")

//      val head: String = nextSubState(".." + (state take 3))
//      val tailHead: String = nextSubState("." + (state take 4))
//
//      val penultimate: String = nextSubState((state takeRight 4) + ".")
//      val ultimate: String = nextSubState((state takeRight 3) + "..")


      val newState: String = newss
      state = newState
    }

  }
}
