package puzzles
import cats._
import cats.implicits._
import cats.effect._
import cats.syntax._
import scala.util.Try

trait AoCApp {
  def run()(implicit cs: ContextShift[IO]): IO[Unit]
}

object AoC extends IOApp {
  val days =
    Array(
      Day1,
      Day2,
      Day3,
      Day4,
      Day5,
      Day6,
      Day7,
      Day8,
      Day9,
      Day10,
      Day11
    )

  def implemented(x: String) =
    Try(x.toInt).toOption match {
      case None    => false
      case Some(k) => k <= days.size
    }

  def run(args: List[String]): IO[ExitCode] =
    for {
      day <- Util.readLineIO("Day: ")
      obj <- day match {
        case x if (implemented(x)) =>
          IO(Try(days(x.toInt - 1)).toOption)
        case "q" => IO(None)
        case _ =>
          Util.printStrLn("Not done yet!") >>
            run(args) >>
            IO(None)
      }
      _ <- obj match {
        case None    => IO.unit
        case Some(o) => o.run >> run(args)
      }
    } yield ExitCode.Success
}
