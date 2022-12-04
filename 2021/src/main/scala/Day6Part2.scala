import scala.io.Source

object Day6Part2 extends App {
  def calculateLanternfish(days: Int, popMap: Map[Int, Long]): Map[Int, Long] =
    days match {
      case 0 => popMap
      case _ =>
        val newPopulation = popMap.flatMap {
          case (0, _) => None
          case (t, c) => Some((t - 1, c))
        }
        val withKids = popMap.get(0) match {
          case None => newPopulation
          case Some(kids) =>
            def incr(by: Long) = (c: Option[Long]) => Some(c.getOrElse(0L) + by)
            newPopulation.updatedWith(6)(incr(kids)).updatedWith(8)(incr(kids))
        }
        calculateLanternfish(days - 1, withKids)
    }
  val source = Source.fromFile("src/input/day6.txt")

  val sourceList: List[String] =
    source.getLines().filterNot(_.isEmpty).toList

  val sources = sourceList.head.split(",").map(_.toInt)
  val popMap = sources.groupBy(identity).view.mapValues(_.size.toLong).toMap
  val result = calculateLanternfish(256, popMap)
  println(result.view.values.sum)
}
