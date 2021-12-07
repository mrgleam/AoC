import scala.annotation.tailrec
import scala.io.Source

object Day7Part1 extends App {
  @tailrec
  def leastFuelCost(
      input: List[Int],
      position: Int,
      min: Int
  ): Int = {
    position match {
      case 0 =>
        min
      case p =>
        val sum = input.map(in => (in - p).abs).sum
        leastFuelCost(
          input,
          p - 1,
          if (min > sum) sum else min
        )
    }
  }
  val source = Source.fromFile("src/input/day7.txt")

  val sourceList: List[String] =
    source.getLines().filterNot(_.isEmpty).toList
  val sources = sourceList.head.split(",").map(_.toInt).toList

  val result = leastFuelCost(sources, sources.max, Int.MaxValue)
  println(result)
}
