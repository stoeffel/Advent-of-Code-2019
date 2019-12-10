package puzzles
import cats.implicits._
import cats.effect._
import cats.effect.concurrent._
import cats.syntax._
import cats.data._
import puzzles.Op.Terminate
import puzzles.Amp.Normal
import puzzles.Amp.FeedbackLook
import puzzles.Op._

object Amp {
  final case class State(pos: Position, memory: Memory, input: Option[Code])

  sealed abstract class Mode extends Product
  final case class Normal() extends Mode
  final case class FeedbackLook() extends Mode

  def possiblePhaseSettings(from: Code, to: Code): NonEmptyList[List[Code]] = {
    val range = Range(from.toInt, to.toInt + 1)
    NonEmptyList
      .fromList(range.permutations.toList)
      .getOrElse(NonEmptyList.of(range))
      .map(x => x.map(_.toLong).toList)
  }

  def maxThrusterSignal(from: Code, to: Code, mode: Mode, memory: Memory)(
      implicit cs: ContextShift[IO]
  ): IO[Code] =
    possiblePhaseSettings(from, to)
      .map(_.map(x => State(Position(0), memory, Some(x))))
      .parTraverse(thrusterSignal(_, mode, None))
      .map(_.toList.flatten.max)

  def thrusterSignal(setting: List[State], mode: Mode, out: Option[Code])(
      implicit cs: ContextShift[IO]
  ): IO[Option[Code]] = setting match {
    case Nil => IO(out)
    case State(pos, memory, input) :: rest =>
      for {
        inputState <- MVar.of[IO, Option[Code]](input)
        result <- IntCode.diagnostic(onInput(inputState, out), memory, pos)
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

  def onInput(inputState: MVar[IO, Option[Code]], out: Option[Code]) =
    for {
      state <- inputState.take
      _ <- inputState.put(out)
    } yield state.getOrElse(out.getOrElse(0: Long))
}
