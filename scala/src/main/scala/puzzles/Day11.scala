package puzzles
import cats.effect._
import cats.syntax.all._
import puzzles.IntCode._
import puzzles.IntCode.Terminate._
import puzzles.Day11.Paint
import puzzles.Day11.Up
import puzzles.Day11.Down
import scala.collection.immutable.Nil
import puzzles.implicits._

object Day11 extends IOApp {
  sealed abstract class Color {
    def toCode(): Code
  }
  final case class Black() extends Color {
    def toCode(): Code = 0
  }
  final case class White() extends Color {
    def toCode(): Code = 1
  }
  object Color {
    def fromCode(x: Code) =
      x match {
        case 1 => White()
        case _ => Black()
      }
  }
  sealed abstract class Action
  final case class Paint() extends Action
  final case class Move() extends Action

  sealed abstract class Direction {
    def left(): Direction
    def right(): Direction
  }
  final case class Up() extends Direction {
    def left(): Direction = Left()
    def right(): Direction = Right()
  }
  final case class Left() extends Direction {
    def left(): Direction = Down()
    def right(): Direction = Up()
  }
  final case class Right() extends Direction {
    def left(): Direction = Up()
    def right(): Direction = Down()
  }
  final case class Down() extends Direction {
    def left(): Direction = Right()
    def right(): Direction = Left()
  }

  type Panel = (Int, Int)
  final case class Panels(
      panels: Map[Panel, Color],
      current: Panel,
      action: Action,
      direction: Direction,
      default: Color
  ) {
    this: Panels =>
    def updated(out: Code): Panels =
      action match {
        case Paint() => paint(out)
        case Move()  => move(out)
      }

    private def paint(out: Code) =
      this.copy(
        panels = panels.get(current) match {
          case None        => panels ++ List(current -> Color.fromCode(out))
          case Some(value) => panels.updated(current, Color.fromCode(out))
        },
        action = Move()
      )

    private def move(out: Code) = {
      val res = (direction, out) match {
        case (Up(), 1)    => current.mapFst(_ + 1) -> direction.right
        case (Up(), _)    => current.mapFst(_ - 1) -> direction.left
        case (Down(), 1)  => current.mapFst(_ - 1) -> direction.right
        case (Down(), _)  => current.mapFst(_ + 1) -> direction.left
        case (Left(), 1)  => current.mapSnd(_ - 1) -> direction.right
        case (Left(), _)  => current.mapSnd(_ + 1) -> direction.left
        case (Right(), 1) => current.mapSnd(_ + 1) -> direction.right
        case (Right(), _) => current.mapSnd(_ - 1) -> direction.left
      }
      this.copy(current = res._1, action = Paint(), direction = res._2)
    }

    def currentColor(): Color = panels.get(current).getOrElse(default)

    def render() = {
      val minX = panels.keys.minBy(_._1)._1
      val minY = panels.keys.minBy(_._2)._2
      var newPanel =
        panels.map(_.mapFst(_.mapBoth(_ + minX.abs, _ + minY.abs)))
      val cols = newPanel.keys.maxBy(_._1)._1 + 1
      val rows = newPanel.keys.maxBy(_._2)._2 + 1
      val row = Array.fill(cols)(" ")
      val grid = Array.fill(rows)(row)
      val whitePanels = newPanel.filter(_._2 == White()).keys.toList
      paintGrid(grid, whitePanels)
        .map(_.toList.mkString(""))
        .toList
        .mkString("\n")
    }

    private def paintGrid(
        grid: Array[Array[String]],
        ps: List[Panel]
    ): Array[Array[String]] =
      ps match {
        case head :: tl =>
          paintGrid(
            grid.updated(head._2, grid(head._2).updated(head._1, "#")),
            tl
          )
        case Nil => grid
      }
  }

  object Panels {
    def init(col: Color) = Panels(Map.empty, (0, 0), Paint(), Up(), col)
  }

  def paint(
      panels: Panels,
      code: Memory,
      pos: Position = Position(0)
  ): IO[Panels] =
    for {
      res <- IntCode.diagnostic(IO(panels.currentColor.toCode), code, pos)
      out <- res match {
        case UnknownOp(e) => throw (new Error(s"Unknown op $e"))
        case End()        => IO(panels)
        case Output(newOut, newCode, newPos) =>
          paint(panels.updated(newOut), newCode, newPos)
      }
    } yield out

  def run(args: List[String]): IO[ExitCode] =
    for {
      code <- IntCode.parse("../input/day-11.txt")
      result1 <- paint(Panels.init(Black()), code).map(_.panels.size)
      result2 <- paint(Panels.init(White()), code).map(_.render)
      _ <- Util.printStrLn(s"AoC 19 - Day 11: $result1")
      _ <- Util.printStrLn(s"AoC 19 - Day 11: \n\n\n$result2")
    } yield (ExitCode.Success)
}
