package example

import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source
import scala.collection.mutable

/**
  * Created by tomas.mccandless on 12/13/18.
  */
class Day13Spec extends FlatSpec with Matchers {

  def crashed(carts: Seq[Cart]): Boolean = {
    carts.map(_.p).distinct.length < carts.length
  }

  def updatePositions(tracks: Array[Array[Char]], carts: Seq[Cart]): Unit = {
    carts.sortBy(c => (c.p.y, c.p.x)) foreach { c =>
      println(c)

      // figure out the next position

      // make sure we get the turning at intersections right

    }
  }

  "day 13" should "solve" in {

    // each entry is a row
    val tracks: Array[Array[Char]] = Source.fromResource("day13_small.txt").getLines.toList.map(_.toCharArray).toArray

    for {
      row <- tracks.indices
      col <- tracks(row).indices
    } {
      print(tracks(row)(col))
      if (col == tracks(row).indices.length - 1) {
        print("\n")
      }
    }

    val dirs: List[Char] = List('>', '<', '^', 'v')

    val carts = for {
      row <- tracks.indices
      col <- tracks(row).indices
      if dirs.contains(tracks(row)(col))
    } yield Cart(Point(col, row), FacingDirection.from(tracks(row)(col)))

    while (!crashed(carts)) {
      updatePositions(tracks, carts)
      println("done with one round")
      Thread.sleep(1000)
    }
  }
}



case class Point(var x: Int, var y: Int)
case class Cart(p: Point, var dir: FacingDirection)


sealed trait FacingDirection {
  val rep: String
  def turn(t: TurnDirection): FacingDirection
}

object FacingDirection {
  def from(c: Char): FacingDirection = c match {
    case '^' => FacingUp
    case '<' => FacingLeft
    case '>' => FacingRight
    case 'v' => FacingDown
  }
}
case object FacingUp extends FacingDirection {
  val rep: String = "^"
  override def turn(t: TurnDirection): FacingDirection = t match {
    case TurnLeft => FacingLeft
    case TurnRight => FacingRight
    case TurnStraight => this
  }
}
case object FacingRight extends FacingDirection {
  val rep: String = ">"
  override def turn(t: TurnDirection): FacingDirection = t match {
    case TurnLeft => FacingUp
    case TurnRight => FacingDown
    case TurnStraight => this
  }
}
case object FacingDown extends FacingDirection {
  val rep: String = "v"
  override def turn(t: TurnDirection): FacingDirection = t match {
    case TurnLeft => FacingRight
    case TurnRight => FacingLeft
    case TurnStraight => this
  }
}
case object FacingLeft extends FacingDirection {
  val rep: String = "<"
  override def turn(t: TurnDirection): FacingDirection = t match {
    case TurnLeft => FacingDown
    case TurnRight => FacingUp
    case TurnStraight => this
  }
}

sealed trait TurnDirection
case object TurnLeft extends TurnDirection
case object TurnStraight extends TurnDirection
case object TurnRight extends TurnDirection

