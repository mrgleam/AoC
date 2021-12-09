import scala.io.Source

object Day9Part1 extends App {
  case class Area(
      currentPoint: Int,
      column: Int,
      topRow: Option[String],
      currentRow: String,
      bottomRow: Option[String]
  ) {
    def Top(): Option[Int] =
      if (topRow.isDefined) topRow.map(_(column)).map(_.asDigit) else None
    def Bottom(): Option[Int] =
      if (bottomRow.isDefined) bottomRow.map(_(column)).map(_.asDigit) else None
    def Left(): Option[Int] =
      if ((column - 1) < 0) None else Some(currentRow(column - 1).asDigit)
    def Right(): Option[Int] = if ((column + 1) >= currentRow.length) None
    else Some(currentRow(column + 1).asDigit)

    def isLowestPoint() = {
      val nearby =
        List(Top(), Bottom(), Left(), Right()).filter(_.isDefined).flatten
      nearby.forall(_ > currentPoint)
    }
  }
  def lowestPoint(
      sourceList: List[String],
      currentRow: Int,
      previousRow: Option[String],
      lowestPointAcc: List[Int]
  ): List[Int] = {
    sourceList match {
      case Nil => lowestPointAcc
      case current :: tail =>
        var newLowerPoint: List[Int] = lowestPointAcc
        current.zipWithIndex.foreach { case (c, i) =>
          val area = Area(c.asDigit, i, previousRow, current, tail.headOption)
          if (area.isLowestPoint()) newLowerPoint = newLowerPoint :+ c.asDigit
        }
        lowestPoint(tail, currentRow + 1, Some(current), newLowerPoint)
    }
  }

  val source = Source.fromFile("src/input/day9.txt")

  val sourceList: List[String] =
    source.getLines().filterNot(_.isEmpty).toList

  val result = lowestPoint(sourceList, 0, None, List())
  println(result.map(_ + 1).sum)
}
