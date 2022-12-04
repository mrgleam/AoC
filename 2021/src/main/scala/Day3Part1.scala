import scala.io.Source

object Day3Part1 extends App {
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
  )

  def gammaCount(input: Array[Char], count: Count): Count = {
    input match {
      case Array(a, b, c, d, e, f, g, h, i, j, k, l) => {
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
  }
  def gammaRateCount(sources: List[String], count: Count): Count = {
    sources match {
      case Nil => count
      case a :: tail =>
        gammaRateCount(tail, gammaCount(a.toCharArray, count))
    }
  }

  def gammaRate(count: Count, limit: Int): Int = {
    count match {
      case Count(a, b, c, d, e, f, g, h, i, j, k, l) =>
        Integer.parseInt(
          (a > limit).compare(false).toString +
            (b > limit).compare(false).toString +
            (c > limit).compare(false).toString +
            (d > limit).compare(false).toString +
            (e > limit).compare(false).toString +
            (f > limit).compare(false).toString +
            (g > limit).compare(false).toString +
            (h > limit).compare(false).toString +
            (i > limit).compare(false).toString +
            (j > limit).compare(false).toString +
            (k > limit).compare(false).toString +
            (l > limit).compare(false).toString,
          2
        )
    }
  }

  def epsilonRate(count: Count, limit: Int): Int = {
    count match {
      case Count(a, b, c, d, e, f, g, h, i, j, k, l) =>
        Integer.parseInt(
          (a <= limit).compare(false).toString +
            (b <= limit).compare(false).toString +
            (c <= limit).compare(false).toString +
            (d <= limit).compare(false).toString +
            (e <= limit).compare(false).toString +
            (f <= limit).compare(false).toString +
            (g <= limit).compare(false).toString +
            (h <= limit).compare(false).toString +
            (i <= limit).compare(false).toString +
            (j <= limit).compare(false).toString +
            (k <= limit).compare(false).toString +
            (l <= limit).compare(false).toString,
          2
        )
    }
  }

  val source = Source.fromFile("src/input/day3.txt")

  val sourceList: List[String] =
    source.getLines().filterNot(_.isEmpty).toList

  val count = {
    gammaRateCount(sourceList, Count(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  }
  val halfOfSourcesSize = sourceList.size / 2

  val gr = gammaRate(count, halfOfSourcesSize)
  val er = epsilonRate(count, halfOfSourcesSize)

  println(s"Answer Part1: ${gr * er}")
}
