import Day7Part2.leastFuelCost
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day7Part2Spec extends AnyFlatSpec with should.Matchers {
  "Day 7 Part 2" should "calculate least fuel cost" in {
    val input = {
      List(
        16, 1, 2, 0, 4, 2, 7, 1, 2, 14
      )
    }

    leastFuelCost(input, input.max, Int.MaxValue) should be(168)
  }
}
