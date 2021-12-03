import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {
  @tailrec
  def increasedCounter(sourceList: List[Int], acc: Int): Int = {
    sourceList match {
      case Nil                     => acc
      case _ :: Nil                => acc
      case p :: c :: Nil if c > p  => increasedCounter(Nil, acc + 1)
      case p :: c :: tail if c > p => increasedCounter(c :: tail, acc + 1)
      case _ :: tail               => increasedCounter(tail, acc)
    }
  }

  def sum3(sourceList: List[Int], acc: List[Int]): List[Int] = {
    sourceList match {
      case Nil           => acc
      case _ :: Nil      => acc
      case _ :: _ :: Nil => acc
      case fst :: snd :: trd :: tail =>
        sum3(List(snd, trd) ::: tail, acc ::: List(fst + snd + trd))
    }
  }
  val source = Source.fromFile("src/input/day1.txt")

  val sourceListPart1: List[Int] =
    source.getLines().filterNot(_.isEmpty).map(_.toInt).toList
  val resultPart1 = increasedCounter(sourceListPart1, 0)
  println(s"Answer Part1: ${resultPart1}")

  val sourceListPart2: List[Int] = sum3(sourceListPart1, List())
  val resultPart2 = increasedCounter(sourceListPart2, 0)
  println(s"Answer Part2: ${resultPart2}")

  source.close()
}
