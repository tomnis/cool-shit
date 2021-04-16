package org.mccandless.coolshit.disjointset

import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source
import scala.collection.mutable

/**
  * Created by tomas.mccandless on 12/13/18.
  */
class Day13Spec extends FlatSpec with Matchers {

  // each entry is a row
  val data: List[String] = Source.fromResource("day13.txt").getLines.toList

  val tracks: Array[Array[Char]] = modifyData(data)


  def modifyData(data: List[String]): Array[Array[Char]] = {
    val tracks: Array[Array[Char]] = data.map(
      _.replaceAll("\\^", "|")
        .replaceAll("v", "|")
        .replaceAll(">", "-")
        .replaceAll("<", "-")
        .toCharArray
    ).toArray

    // FIX may need a fix here

    tracks
  }

  def crashed(carts: mutable.Buffer[Cart]): Boolean = {
    val result = carts.map(_.p).distinct.length < carts.length

    if (result) {
      val crashedCarts = carts.groupBy(_.p).filter(_._2.length > 1)
      println(crashedCarts)
      crashedCarts.values.toList.foreach(cs => cs foreach { c =>
        println(s"removing $c")
        carts -= c
      })

    }
    result
  }

  def updatePositions(carts: mutable.Buffer[Cart]): Unit = {
    println(s"${carts.length} carts remaining")
    if (carts.length == 1) {
      return
    }
    carts.sortBy(c => (c.p.y, c.p.x)) foreach { c =>
//      println(c)

      // figure out the next position
      c.dir match {
        case FacingUp =>
          c.p.y -= 1
          val newSpot: Char = tracks(c.p.y)(c.p.x)
          if (newSpot == '\\') {
            c.dir = FacingLeft
          }
          else if (newSpot == '/') {
            c.dir = FacingRight
          }
          else if (newSpot == '+') {
            val t: TurnDirection = c.turn
            c.dir = c.dir.turn(t)
          }
        case FacingLeft =>
          c.p.x -= 1
          val newSpot: Char = tracks(c.p.y)(c.p.x)
          if (newSpot == '\\') {
            c.dir = FacingUp
          }
          else if (newSpot == '/') {
            c.dir = FacingDown
          }
          else if (newSpot == '+') {
            val t: TurnDirection = c.turn
            c.dir = c.dir.turn(t)
          }
        case FacingRight =>
          c.p.x += 1
          val newSpot: Char = tracks(c.p.y)(c.p.x)
          if (newSpot == '\\') {
            c.dir = FacingDown
          }
          else if (newSpot == '/') {
            c.dir = FacingUp
          }
          else if (newSpot == '+') {
            val t: TurnDirection = c.turn
            c.dir = c.dir.turn(t)
          }
        case FacingDown =>
          c.p.y += 1
          val newSpot: Char = tracks(c.p.y)(c.p.x)
          if (newSpot == '\\') {
            c.dir = FacingRight
          }
          else if (newSpot == '/') {
            c.dir = FacingLeft
          }
          else if (newSpot == '+') {
            val t: TurnDirection = c.turn
            c.dir = c.dir.turn(t)
          }
      }

      // make sure we get the turning at intersections right
      println(s"${carts.length} carts remaining")

      if (crashed(carts)) {
        println(s"something crashed")
        if (carts.length == 1) {
          println(s"only 1 cart left")
          println(carts)
          return
        }
      }
    }
  }

  "day 13" should "solve" in {


    for {
      row <- data.indices
      col <- data(row).indices
    } {
      print(data(row)(col))
      if (col == data(row).indices.length - 1) {
        print("\n")
      }
    }

    val dirs: List[Char] = List('>', '<', '^', 'v')

    val immutableCarts: Seq[Cart] = for {
      row <- data.indices
      col <- data(row).indices
      if dirs.contains(data(row)(col))
    } yield Cart(Point(col, row), FacingDirection.from(data(row)(col)))

    val carts: mutable.Buffer[Cart] = mutable.ListBuffer.empty
    immutableCarts.foreach(c => carts += c)

    while (carts.length > 1) {
      updatePositions(carts)
//      println("done with one round")
//      Thread.sleep(100)
    }
  }
}



case class Point(var x: Int, var y: Int)
case class Cart(p: Point, var dir: FacingDirection) {
  var numIntersections: Int = 0
  def turn: TurnDirection = {
    val t = this.numIntersections % 3 match {
      case 0 => TurnLeft
      case 1 => TurnStraight
      case 2 => TurnRight
    }
    this.numIntersections += 1
    t
  }
}


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

