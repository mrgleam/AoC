import scala.io.Source

object Day5Part2 extends App {
  case class Coordinate(x1: Int, y1: Int, x2: Int, y2: Int)

  def createCoordinates(sources: List[String]): List[Coordinate] = {
    sources.map(s => {
      val split = s.split(" -> ")
      val startCoordinate = split.head.split(",").map(_.toInt)
      val endCoordinate = split(1).split(",").map(_.toInt)
      Coordinate(
        startCoordinate(0),
        startCoordinate(1),
        endCoordinate(0),
        endCoordinate(1)
      )
    })
  }

  def fillDiagrams(
      coordinates: List[Coordinate],
      diagrams: Array[Array[Int]]
  ): Array[Array[Int]] = {
    def fillDiagonalLine(
        startX: Int,
        startY: Int,
        endX: Int,
        endY: Int,
        currentX: Int,
        currentY: Int,
        diagrams: Array[Array[Int]]
    ): Array[Array[Int]] = {
      (currentX, currentY) match {
        case (currentX, currentY) if currentX == endX && currentY == endY =>
          diagrams(currentY)(currentX) = diagrams(currentY)(currentX) + 1
          diagrams
        case _ =>
          diagrams(currentY)(currentX) = diagrams(currentY)(currentX) + 1
          fillDiagonalLine(
            startX,
            startY,
            endX,
            endY,
            currentX + 1,
            if (startY > endY) currentY - 1 else currentY + 1,
            diagrams
          )

      }
    }

    def fillDiagram(
        coordinate: Coordinate,
        diagrams: Array[Array[Int]]
    ): Array[Array[Int]] = {
      coordinate match {
        case Coordinate(x1, y1, x2, y2) if y1 == y2 =>
          (if (x1 < x2) x1 to x2
           else x2 to x1).foreach(x => diagrams(y1)(x) = diagrams(y1)(x) + 1)
          diagrams

        case Coordinate(x1, y1, x2, y2) if x1 == x2 =>
          (if (y1 < y2) y1 to y2 else y2 to y1).foreach(y =>
            diagrams(y)(x1) = diagrams(y)(x1) + 1
          )
          diagrams

        case Coordinate(x1, y1, x2, y2) if (x1 - x2).abs == (y1 - y2).abs =>
          if (x1 < x2) fillDiagonalLine(x1, y1, x2, y2, x1, y1, diagrams)
          else fillDiagonalLine(x2, y2, x1, y1, x2, y2, diagrams)

        case _ => diagrams
      }
    }

    coordinates match {
      case Nil => diagrams
      case h :: tail =>
        val newDiagrams = fillDiagram(h, diagrams)
        fillDiagrams(tail, newDiagrams)
    }
  }

  def getPoint(diagrams: Array[Array[Int]]): Int = {
    diagrams.flatMap(diagrams => diagrams.filter(diagram => diagram > 1)).length
  }

  val source = Source.fromFile("src/input/day5.txt")

  val sourceList: List[String] =
    source.getLines().filterNot(_.isEmpty).toList

  val coordinates = createCoordinates(sourceList)
  val diagrams = Array.ofDim[Int](1000, 1000)

  val newDiagrams = fillDiagrams(coordinates, diagrams)
  val result = getPoint(newDiagrams)
  println(result)
}
