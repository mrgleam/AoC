import scala.io.Source

object Day8Part1 extends App {
  val source = Source.fromFile("src/input/day8.txt")

  val sourceList: List[String] = {
    source.getLines().filterNot(_.isEmpty).toList
  }
  val sources = sourceList.map(sources => {
    val tails = sources.split(" \\| ").tail
    val splitTails: Array[String] = tails.flatMap(_.split(" "))
    splitTails
  })

  val x: Seq[String] = sources.flatMap(s =>
    s.filter(s =>
      s.length == 2 || s.length == 3 || s.length == 4 || s.length == 7
    )
  )

  println(x.length)
}
