package org.mccandless.coolshit.disjointset

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

/**
  *
  * Created by tdm on 12/15/18.
  */
class Day14Spec extends FlatSpec with Matchers {

  /** Creates new recipes from the digits of the sum of the current recipes' scores.
    *
    * With the current recipes' scores of 3 and 7, their sum is 10, i
    * two new recipes would be created:
    *   the first with score 1
    *   and the second with score 0.
    *
    * If the current recipes' scores were 2 and 3, the sum, 5,
    * would only create one recipe (with a score of 5) with its single digit.
    */
  def combineRecipes(r1: Recipe, r2: Recipe): Unit = {
    var totalScore: Int = r1.qualityScore + r2.qualityScore
//      .toString.map(c => Recipe(c.toString.toInt))
    val newScores: Seq[Int] = totalScore.toString.map(_.toString.toInt)

    newScores foreach { newScore =>
      board += Recipe(newScore)
      lastScores += newScore.toString
      if (lastScores.length > targetLengthStr.length) {
        lastScores = lastScores drop 1
      }
      if (lastScores == targetLengthStr) {
        println(s"board length: ${board.length - targetLengthStr.length}")
        System.exit(0)
      }
    }


  }


  /**
    * each Elf picks a new current recipe.
    * To do this, the Elf
    * steps forward through the scoreboard a number of recipes equal to 1 plus the score of their current recipe.
    *
    * So, after the first round, the first Elf moves forward 1 + 3 = 4 times,
    * while the second Elf moves forward 1 + 7 = 8 times. If they run out of recipes,
    * they loop back around to the beginning.
    *
    * should be after new recipes have been created
    * @param e
    * @return
    */
  def pickNewRecipe(e: Elf): Int = {
    val newIndex: Int = (e.recipeIdx + board(e.recipeIdx).qualityScore + 1) % board.length
//    e.recipeIdx = newIndex
    newIndex
  }

  def step(e1: Elf, e2: Elf): Seq[Recipe] = {
//    val news: Seq[Recipe] = combineRecipes(board(e1.recipeIdx), board(e2.recipeIdx))
//    board ++= news
    combineRecipes(board(e1.recipeIdx), board(e2.recipeIdx))
    e1.recipeIdx = pickNewRecipe(e1)
    e2.recipeIdx = pickNewRecipe(e2)
    board
  }


  def printState(elves: List[Elf]): Unit = {

    board.zipWithIndex foreach { case (rec, i) =>
        elves.find(_.recipeIdx == i) match {
          case Some(e) => print(s"${e.beginChar}${rec.qualityScore}${e.endChar} ")
          case None => print(s"${rec.qualityScore} ")
        }
    }
    println("")
  }


  val targetLength: Int =236021
  val targetLengthStr: String = targetLength.toString
  val board: mutable.Buffer[Recipe] = new mutable.ArrayBuffer[Recipe] { override val initialSize: Int = Int.MaxValue }
  var lastScores: String = "37"
  board += Recipe(3)
  board += Recipe(7)


  "day 14" should "solve" in {
    val elves: List[Elf] = List(Elf(1, '(', ')', 0), Elf(2, '[', ']', 1))


    var stop: Boolean = false

    while (!stop) {
//      printState(elves)
      step(elves.head, elves.tail.head)
      if (board.length % 10000 == 0) {
        println(board.length)
      }
      if (lastScores == targetLengthStr) {
        println(s"answer: ${board.slice(targetLength, targetLength + 10).map(_.qualityScore).mkString("")}")
        println(s"board length: ${board.length - targetLengthStr.length}")
        stop = true
      }
//      Thread.sleep(1000)
    }

  }
}

case class Elf(id: Int, beginChar: Char, endChar: Char, var recipeIdx: Int)

case class Recipe(qualityScore: Int)
