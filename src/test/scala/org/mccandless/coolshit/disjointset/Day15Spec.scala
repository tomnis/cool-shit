package org.mccandless.coolshit.disjointset

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  *
  * Created by tdm on 12/16/18.
  */
class Day15Spec extends FlatSpec with Matchers {

  import Day15Spec._

  val wall: Char = '#'
  val cavern: Char = '.'


  def printState(): Unit = {
    for {
      row <- board.indices
      col <- board(row).indices
    } {
      print(board(row)(col))
      if (col == board(row).length - 1) {
        println("")
      }
    }

  }


  "day 15" should "solve" in {


    printState()
    // outcome = number of full rounds that were completed (not counting the round in which combat ends)
    // multiplied by the sum of the hit points of all remaining units at the moment combat ends
  }

}


object Day15Spec {

  val input: List[String] = Source.fromResource("day15_small.txt").getLines.toList

  val board: mutable.ArrayBuffer[mutable.ArrayBuffer[Char]] = {
    val x: mutable.ArrayBuffer[mutable.ArrayBuffer[Char]] = mutable.ArrayBuffer.empty
    for {
      row <- input.indices
      col <- input(row).indices
    } {
      if (x.length <= row) {
        x += mutable.ArrayBuffer.empty
      }
      x(row) += input(row).charAt(col)
    }
    x
  }

  // TODO do we need this explicit rep of all the units?
  // bookkeeping
//  val units: mutable.Map[Point, Unit] = {
//    val x: mutable.Map[Point, Unit] = mutable.Map.empty
//    for {
//      row <- input.indices
//      col <- input(row).indices
//    } {
//
//      if (input(row).charAt(col) == 'G') x += (Point(col, row) -> Goblin(Point(col, row)))
//      else if (input(row).charAt(col) == 'E') x += (Point(col, row) -> Elf(Point(col, row)))
//    }
//    x
//  }
}


//case class Point(x: Int, y: Int)

sealed trait BattleUnit {

  val rep: String


  val attackPower: Int = 3
  var hitPoints: Int = 200

  var position: Point

  /**
    * resolving all of its actions before the next unit's turn begins.
    * On each unit's turn, it tries to move into range of an enemy (if it isn't already) and then attack (if it is in range).
    * @return
    */
  def turn(): Unit = {
    // identify all possible targets

    // identifies all of the open squares (.) that are in range of each target
    // these are the squares which are adjacent (immediately up, down, left, or right

    // Alternatively, the unit might already be in range of a target.
    // If the unit is not already in range of a target,
    // and there are no open squares which are in range of a target, the unit ends its turn.

    // If the unit is already in range of a target, it does not move, but continues its turn with an attack.
    // Otherwise, since it is not in range of a target, it moves.
    ???
  }

  def move(): Unit = {
    // the unit first considers the squares that are in range and determines which of those squares it could reach
    // in the fewest steps. A step is a single movement to any adjacent (immediately up, down, left, or right) open (.) square.

    // any of the squares that are in range, it ends its turn.

    // If multiple squares are in range and tied for being reachable in the fewest steps, the square which is first in reading order is chosen. For example:
  }


  def attack(): Unit = {
    // the unit first determines all of the targets that are in range of it by being immediately adjacent to it.
    // If there are no such targets, the unit ends its turn.
    // Otherwise, the adjacent target with the fewest hit points is selected;
    // in a tie, the adjacent target with the fewest hit points which is first in reading order is selected.
  }


  def die(): Unit = {
    // its square becomes . and it takes no further turns.
  }
}

case class Goblin(override var position: Point) extends BattleUnit {
  override val rep: String = "G"
}
//case class Elf(override var position: Point) extends BattleUnit {
//  override val rep: String = "E"
//}
