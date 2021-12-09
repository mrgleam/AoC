import Day9Part1.lowestPoint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day9Part1Spec extends AnyFlatSpec with should.Matchers {
  "Day 9 Part 1" should "calculate least fuel cost" in {
    val input = {
      List(
        "2199943210",
        "3987894921",
        "9856789892",
        "8767896789",
        "9899965678"
      )
    }

    lowestPoint(input, 0, None, List()) should be(List(1, 0, 5, 5))
  }
}
