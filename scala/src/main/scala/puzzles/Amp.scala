package puzzles
import cats.implicits._
import cats.effect._
import cats.effect.concurrent._
import cats.syntax._
import cats.data._
import puzzles.Op.Terminate
import puzzles.Amp.Normal
import puzzles.Amp.FeedbackLook

object Amp {
  type Code = Array[Int]
  final case class State(pos: Int, code: Code, input: Option[Int])

  sealed abstract class Mode extends Product
  final case class Normal() extends Mode
  final case class FeedbackLook() extends Mode

  def possiblePhaseSettings(from: Int, to: Int): NonEmptyList[List[Int]] = {
    val range = Range(from, to + 1)
    NonEmptyList
      .fromList(range.permutations.toList)
      .getOrElse(NonEmptyList.of(range))
      .map(x => x.toList)
  }

  def maxThrusterSignal(from: Int, to: Int, mode: Mode, code: Code)(
      implicit cs: ContextShift[IO]
  ): IO[Int] =
    possiblePhaseSettings(from, to)
      .map(_.map(x => State(0, code, Some(x))))
      .parTraverse(thrusterSignal(_, mode, None))
      .map(_.toList.flatten.max)

  def thrusterSignal(setting: List[State], mode: Mode, out: Option[Int])(
      implicit cs: ContextShift[IO]
  ): IO[Option[Int]] = setting match {
    case Nil => IO(out)
    case State(pos, code, input) :: rest =>
      for {
        inputState <- MVar.of[IO, Option[Int]](input)
        result <- IntCode.diagnostic(onInput(inputState, out), code, pos)
        finalResult <- result match {
          case Terminate.UnknownOp(e) => throw (new Error(s"Unknown op $e"))
          case Terminate.End()        => IO(out)
          case Terminate.Output(newOut, newCode, newPos) =>
            mode match {
              case Normal() =>
                thrusterSignal(rest, mode, Some(newOut))
              case FeedbackLook() =>
                thrusterSignal(
                  rest :+ State(newPos, newCode, None),
                  mode,
                  Some(newOut)
                )
            }
        }
      } yield finalResult
  }

  def onInput(inputState: MVar[IO, Option[Int]], out: Option[Int]) =
    for {
      state <- inputState.take
      _ <- inputState.put(out)
    } yield state.getOrElse(out.getOrElse(0))
}
