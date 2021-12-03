import scala.annotation.tailrec
import scala.io.Source

sealed trait Command
object Forward extends Command
object Down extends Command
object Up extends Command

object Day2Part1 extends App {
  case class CommandUnit(position: Command, unit: Int)
  case class Position(horizontal: Int, depth: Int)

  @tailrec
  def calculateHorizontalAndDepth(
      commands: List[CommandUnit],
      position: Position
  ): Position = {
    commands match {
      case Nil => position
      case CommandUnit(Forward, unit) :: tail =>
        calculateHorizontalAndDepth(
          tail,
          Position(position.horizontal + unit, position.depth)
        )
      case CommandUnit(Up, unit) :: tail =>
        calculateHorizontalAndDepth(
          tail,
          Position(position.horizontal, position.depth - unit)
        )
      case CommandUnit(Down, unit) :: tail =>
        calculateHorizontalAndDepth(
          tail,
          Position(position.horizontal, position.depth + unit)
        )
    }
  }

  val source = Source.fromFile("src/input/day2.txt")

  val sourceListPart1: List[CommandUnit] =
    source
      .getLines()
      .filterNot(_.isEmpty)
      .map { line =>
        line.split(" ") match {
          case Array("forward", unit) => CommandUnit(Forward, unit.toInt)
          case Array("up", unit)      => CommandUnit(Up, unit.toInt)
          case Array("down", unit)    => CommandUnit(Down, unit.toInt)
        }
      }
      .toList

  val positionPart1 = {
    calculateHorizontalAndDepth(sourceListPart1, Position(0, 0))
  }
  val resultPart1 = positionPart1.horizontal * positionPart1.depth
  println(s"Answer Part1: ${resultPart1}")
}
