package example

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tomas.mccandless on 12/12/18.
  */
class Day11Spec extends FlatSpec with Matchers {

  val serialNumber: Int = 9005

  case class FuelCell(x: Int, y: Int) {
    val rackId: Int = x + 10
    val powerLevel: Long = {
      var p = this.rackId * this.y
      p += serialNumber
      p *= this.rackId
      p = p / 100 % 10
      if (p < 0) {
        throw new RuntimeException("it was zero")
      }
      p -= 5
      p
    }
  }


  "day 11" should "day 11" in {

    val c = FuelCell(3, 5)
    println(c.powerLevel)

    val grid: Map[(Int, Int), FuelCell] = (for {
      y <- 1 to 300
      x <- 1 to 300
    } yield (x, y) -> FuelCell(x, y)).toMap


    var maxPower: Long = Long.MinValue
    var maxPowerLocation: (Int, Int) = (Int.MinValue, Int.MinValue)
    var maxSize: Int = 0

    // top left corner bounds
    for {
      size <- (1 to 300)
      y <- 1 to (300 - size + 1)
      x <- 1 to (300 - size + 1)
    } {
      val cells = for {
        yp <- y to (y + size -1)
        xp <- x to (x + size -1)
      } yield grid(xp, yp)

      val sum = cells.map(_.powerLevel).sum

      if (sum > maxPower) {
        println(s"found new max $sum at $x, $y size = $size")
        maxSize = size
        maxPower = sum
        maxPowerLocation = (x, y)
      }
    }


    println(s"max $maxPower at $maxPowerLocation size = $maxSize")
  }

}
