import scala.annotation.tailrec
import scala.io.Source

object Day3Part2 extends App {
  case class Count(
      a: Int,
      b: Int,
      c: Int,
      d: Int,
      e: Int,
      f: Int,
      g: Int,
      h: Int,
      i: Int,
      j: Int,
      k: Int,
      l: Int
  ) {
    def toList: List[Int] = {
      List(a, b, c, d, e, f, g, h, i, j, k, l)
    }
  }

  def count(input: Array[Char], count: Count): Count = {
    input match {
      case Array(a, b, c, d, e, f, g, h, i, j, k, l) =>
        Count(
          count.a + a.asDigit,
          count.b + b.asDigit,
          count.c + c.asDigit,
          count.d + d.asDigit,
          count.e + e.asDigit,
          count.f + f.asDigit,
          count.g + g.asDigit,
          count.h + h.asDigit,
          count.i + i.asDigit,
          count.j + j.asDigit,
          count.k + k.asDigit,
          count.l + l.asDigit
        )
    }
  }

  def rateCount(sources: List[String], c: Count): Count = {
    sources match {
      case Nil => c
      case a :: tail =>
        rateCount(tail, count(a.toCharArray, c))
    }
  }

  @tailrec
  def oxygenRate(
      sources: List[String],
      count: Count,
      limit: Float,
      countIndex: Int,
      acc: List[String]
  ): List[String] = {
    if (sources.size == 1) {
      acc
    } else {
      countIndex match {
        case 12 => acc
        case _ =>
          val countList = count.toList
          val filteredSources = sources
            .filter(s => {
              if (countList(countIndex) >= limit) {
                s.charAt(countIndex).compare('1') == 0
              } else {
                s.charAt(countIndex).compare('0') == 0
              }
            })
          oxygenRate(
            filteredSources,
            rateCount(
              filteredSources,
              Count(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            ),
            filteredSources.size / 2f,
            countIndex + 1,
            filteredSources
          )
      }
    }

  }

  @tailrec
  def co2Rate(
      sources: List[String],
      count: Count,
      limit: Float,
      countIndex: Int,
      acc: List[String]
  ): List[String] = {
    if (sources.size == 1) {
      acc
    } else {
      countIndex match {
        case 12 => acc
        case _ =>
          val countList = count.toList
          val filteredSources = sources
            .filter(s => {

              if (countList(countIndex) < limit) {
                s.charAt(countIndex).compare('1') == 0
              } else {
                s.charAt(countIndex).compare('0') == 0
              }

            })
          co2Rate(
            filteredSources,
            rateCount(
              filteredSources,
              Count(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            ),
            filteredSources.size / 2f,
            countIndex + 1,
            filteredSources
          )
      }
    }

  }

  val source = Source.fromFile("src/input/day3.txt")

  val sourceList: List[String] =
    source.getLines().filterNot(_.isEmpty).toList

  val halfOfSourcesSize = sourceList.size / 2f

  val oRate = oxygenRate(
    sourceList,
    rateCount(
      sourceList,
      Count(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    ),
    halfOfSourcesSize,
    0,
    List()
  )

  println(oRate)

  val coRate = co2Rate(
    sourceList,
    rateCount(
      sourceList,
      Count(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    ),
    halfOfSourcesSize,
    0,
    List()
  )

  println(coRate)
  val x = Integer.parseInt(oRate.head, 2)
  val y = Integer.parseInt(coRate.head, 2)
  println(s"Answer Part2: ${x * y}")
}
