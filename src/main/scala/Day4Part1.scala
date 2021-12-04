import scala.io.Source

object Day4Part1 extends App {
  def createTables(
      data: List[String],
      counterRow: Int,
      acc: Array[Array[Array[String]]],
      accTable: Array[Array[String]]
  ): Array[Array[Array[String]]] = {
    def createRow(
        input: String
    ): Array[String] = {
      input.replaceFirst("^ *", "").replace("  ", " ").split(" ")
    }
    data match {
      case Nil => acc
      case h :: tail =>
        val row = createRow(h)
        val newAccTable = accTable :+ row
        createTables(
          tail,
          if (counterRow == 4) 0 else counterRow + 1,
          if (counterRow == 4) acc :+ newAccTable else acc,
          if (counterRow == 4) Array() else newAccTable
        )
    }
  }
  def createBingo(
      sourceList: List[String]
  ): (List[String], Array[Array[Array[String]]]) = {
    sourceList match {
      case h :: tail =>
        (
          h.split(",").toList,
          createTables(tail, 0, Array(), Array())
        )
    }
  }
  val source = Source.fromFile("src/input/day4.txt")

  val sourceList: List[String] =
    source.getLines().filterNot(_.isEmpty).toList

  val (bingoRunner, bingoTable) = createBingo(sourceList)

  var markBingo = bingoTable
  var tableBingo: Array[Array[String]] = Array()
  var markRunner = ""

  bingoRunner foreach (runner => {
    markBingo = markBingo.map(table =>
      table.map(row =>
        row.map(member => if (member.equals(runner)) "x" else member)
      )
    )
    markBingo.foreach(table =>
      table.foreach(row => {
        val newRow = row
        val filtered = newRow.filterNot(_.equals("x"))
        if (filtered.length == 0) {
          if (tableBingo.isEmpty) {
            tableBingo = table
            markRunner = runner
          }
        }
      })
    )
    markBingo.foreach(table =>
      table.transpose.foreach(row => {
        val newRow = row
        val filtered = newRow.filterNot(_.equals("x"))
        if (filtered.length == 0) {
          if (tableBingo.isEmpty) {
            tableBingo = table
            markRunner = runner
          }
        }
      })
    )
  })
  tableBingo.map(_.mkString(" ")).foreach(println)
  val result = tableBingo.flatMap(table => table.filterNot(_.equals("x")))
  println(result.map(_.toInt).sum)
  println(markRunner.toInt)
  println(result.map(_.toInt).sum * markRunner.toInt)
}
