import scala.io.Source

object Day8Part2 extends App {

  def superSet(a: List[Char], b: List[Char]): Boolean = b.forall(a.contains(_))
  def subSet(b: List[Char], a: List[Char]): Boolean = b.forall(a.contains(_))

  def simpleSegmentDisplay(signals: List[String]): Map[String, String] = {
    signals.map {
      case s if s.length == 2 => s -> "1"
      case s if s.length == 3 => s -> "7"
      case s if s.length == 4 => s -> "4"
      case s if s.length == 7 => s -> "8"
      case s                  => s -> ""
    }.toMap
  }

  def complexSegmentDisplay(
      complexSignals: List[String],
      simplePatterns: Map[String, String],
      acc: Map[String, String]
  ): Map[String, String] = {
    def matchComplexSegmentDisplay(
        signal: String
    ): Map[String, String] = {
      signal match {
        case s
            if s.length == 6 && superSet(
              s.toList,
              simplePatterns.getOrElse("4", "").toList
            ) =>
          Map(s -> "9")
        case s
            if s.length == 6 && superSet(
              s.toList,
              simplePatterns.getOrElse("1", "").toList
            ) =>
          Map(s -> "0")
        case s if s.length == 6 => Map(s -> "6")
        case s
            if s.length == 5 && superSet(
              s.toList,
              simplePatterns.getOrElse("1", "").toList
            ) =>
          Map(s -> "3")
        case s
            if s.length == 5 && subSet(
              s.toList,
              acc.map(_.swap).getOrElse("9", "").toList
            ) =>
          Map(s -> "5")
        case s if s.length == 5 => Map(s -> "2")
        case _                  => Map()
      }
    }

    complexSignals.sortBy(_.length).reverse match {
      case Nil => acc
      case h :: tail =>
        complexSegmentDisplay(
          tail,
          simplePatterns,
          acc ++ matchComplexSegmentDisplay(h)
        )
    }
  }

  def getSignalsAndOutputs(sourceList: List[String]) = {
    sourceList.map(sources => {
      val split = sources.split(" \\| ")
      val signals = split.head.split(" ").map(_.sorted)
      val output: Array[String] = split.tail.flatMap(_.split(" ")).map(_.sorted)
      (signals, output)
    })
  }

  def decodeSignals(sources: List[(Array[String], Array[String])]) = {
    sources
      .map(s => {
        val (signals, outputs) = s
        val simpleSignals =
          simpleSegmentDisplay(signals.toList).filterNot(_._2.isEmpty)
        val simplePatterns = simpleSignals.map(_.swap)

        val complexSignals = complexSegmentDisplay(
          signals
            .filterNot(signal => simpleSignals.contains(signal))
            .toList,
          simplePatterns,
          Map()
        )

        val decodeSignals = simpleSignals ++ complexSignals

        val result = outputs.flatMap(o => decodeSignals.get(o))
        result.toList.mkString.toInt
      })
  }

  val source = Source.fromFile("src/input/day8.txt")

  val sourceList: List[String] = {
    source.getLines().filterNot(_.isEmpty).toList
  }
  val sources = getSignalsAndOutputs(sourceList)

  val result = decodeSignals(sources)
  println(result.sum)

}
