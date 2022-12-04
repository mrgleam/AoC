import scala.annotation.tailrec
import scala.io.Source

object Day2Part2 extends App {
  sealed trait Command
  object Forward extends Command
  object Down extends Command
  object Up extends Command

  case class CommandUnit(position: Command, unit: Int)
  case class Position(horizontal: Int, depth: Int, aim: Int)

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
          Position(
            position.horizontal + unit,
            position.depth + (position.aim * unit),
            position.aim
          )
        )
      case CommandUnit(Up, unit) :: tail =>
        calculateHorizontalAndDepth(
          tail,
          Position(position.horizontal, position.depth, position.aim - unit)
        )
      case CommandUnit(Down, unit) :: tail =>
        calculateHorizontalAndDepth(
          tail,
          Position(position.horizontal, position.depth, position.aim + unit)
        )
    }
  }

  val source = Source.fromFile("src/input/day2.txt")

  val sourceList: List[CommandUnit] =
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

  val position = {
    calculateHorizontalAndDepth(sourceList, Position(0, 0, 0))
  }
  val result = position.horizontal * position.depth
  println(s"Answer Part2: ${result}")
}
