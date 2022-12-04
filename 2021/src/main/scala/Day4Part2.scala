import scala.io.Source

object Day4Part2 extends App {
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
  var lastTableBingo: Array[Array[String]] = Array()
  var markRunner = ""

  bingoRunner foreach (runner => {
    markBingo = markBingo.map(table =>
      table.map(row =>
        row.map(member => if (member.equals(runner)) "x" else member)
      )
    )

    val newMarkBingo = markBingo
    var removeIndex: Array[Int] = Array()
    newMarkBingo.zipWithIndex.foreach { case (table, index) =>
      table.foreach { row =>
        {
          val newRow = row
          val filtered = newRow.filterNot(_.equals("x"))
          if (filtered.length == 0) {
            removeIndex = removeIndex :+ index
          }
        }
      }
    }

    removeIndex.reverse.foreach(index => {
      if (markBingo.length > 1) {
        val (x, y) = markBingo.splitAt(index)
        markBingo = if (!y.isEmpty) x ++ y.tail else x
      } else {
        if (lastTableBingo.length == 0) {
          lastTableBingo = markBingo.head.clone()
          markRunner = runner
        }
      }
    })

    removeIndex = Array()
    newMarkBingo.zipWithIndex.foreach { case (table, index) =>
      table.transpose.foreach { row =>
        {
          val newRow = row
          val filtered = newRow.filterNot(_.equals("x"))
          if (filtered.length == 0) {
            removeIndex = removeIndex :+ index
          }
        }
      }
    }

    removeIndex.reverse.foreach(index => {
      if (markBingo.length > 1) {
        val (x, y) = markBingo.splitAt(index)
        markBingo = if (!y.isEmpty) x ++ y.tail else x
      } else {
        if (lastTableBingo.length == 0) {
          lastTableBingo = markBingo.head.clone()
          markRunner = runner
        }
      }
    })

  })
  lastTableBingo.map(_.mkString(" ")).foreach(println)
  val result = lastTableBingo.flatMap(table => table.filterNot(_.equals("x")))
  println(result.map(_.toInt).sum)
  println(markRunner.toInt)
  println(result.map(_.toInt).sum * markRunner.toInt)
}
