import Day6Part1.calculateLanternfish
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day6Part1Spec extends AnyFlatSpec with should.Matchers {
  "Lantern fish" should "count internal timer" in {
    val input = {
      List(
        3,
        4,
        3,
        1,
        2
      ).map(_.toShort)
    }

    val dataTest =
      List(
        (1, List(2, 3, 2, 0, 1), 5),
        (2, List(1, 2, 1, 6, 0, 8), 6),
        (
          18,
          List(6, 0, 6, 4, 5, 6, 0, 1, 1, 2, 6, 0, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7,
            8, 8, 8, 8),
          26
        )
      )

    dataTest.foreach { case (interval, expectedList, expectedSum) =>
      val result = calculateLanternfish(input, interval)
      result should be(expectedList)
      result.length should be(expectedSum)
    }
  }
}
