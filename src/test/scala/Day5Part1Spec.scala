import org.scalatest._
import flatspec._
import matchers._

class Day5Part1Spec extends AnyFlatSpec with should.Matchers {
  "coordinate" should "fill counter number to diagrams" in {
    val input =
      List(
        "0,9 -> 5,9",
        "8,0 -> 0,8",
        "9,4 -> 3,4",
        "2,2 -> 2,1",
        "7,0 -> 7,4",
        "6,4 -> 2,0",
        "0,9 -> 2,9",
        "3,4 -> 1,4",
        "0,0 -> 8,8",
        "5,5 -> 8,2"
      )
    val coordinates = Day5Part1.createCoordinates(input)
    val filledDiagrams =
      Day5Part1.fillDiagrams(coordinates, Array.ofDim[Int](10, 10))
    filledDiagrams(0) should be(Array(0, 0, 0, 0, 0, 0, 0, 1, 0, 0))
    filledDiagrams(1) should be(Array(0, 0, 1, 0, 0, 0, 0, 1, 0, 0))
    filledDiagrams(2) should be(Array(0, 0, 1, 0, 0, 0, 0, 1, 0, 0))
    filledDiagrams(3) should be(Array(0, 0, 0, 0, 0, 0, 0, 1, 0, 0))
    filledDiagrams(4) should be(Array(0, 1, 1, 2, 1, 1, 1, 2, 1, 1))
    filledDiagrams(5) should be(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    filledDiagrams(6) should be(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    filledDiagrams(7) should be(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    filledDiagrams(8) should be(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    filledDiagrams(9) should be(Array(2, 2, 2, 1, 1, 1, 0, 0, 0, 0))
    Day5Part1.getPoint(filledDiagrams) should be(5)
  }
}
