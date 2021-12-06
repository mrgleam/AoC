import scala.annotation.tailrec

import scala.io.Source

object Day6Part1 extends App {
  @tailrec
  def calculateLanternfish(initial: List[Short], interval: Int): List[Short] = {
    def calculateIntervalTimer(initial: List[Short]): List[Short] = {
      val isZero = initial.filter(_ == 0)

      val minusOneDays = initial.map {
        case 0 => 6
        case x => (x - 1)
      } map (_.toShort)
      minusOneDays ++ isZero.map(_ => 8)
    }
    interval match {
      case 0 => initial
      case _ =>
        calculateLanternfish(calculateIntervalTimer(initial), interval - 1)
    }
  }

  val source = Source.fromFile("src/input/day6.txt")

  val sourceList: List[String] =
    source.getLines().filterNot(_.isEmpty).toList

  val sources: List[Short] = sourceList.head.split(",").map(_.toShort).toList

  val result = calculateLanternfish(sources, 80)
  println(result.length)
}
