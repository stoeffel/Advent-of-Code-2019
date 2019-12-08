package puzzles
import cats.implicits._
import cats.effect._
import cats.effect.concurrent._
import cats.syntax._
import cats.data._

object Amp {
  final case class Setting(value: List[Int])
  def possiblePhaseSettings(from: Int, to: Int): NonEmptyList[Setting] = {
    val range = Range(from, to + 1)
    NonEmptyList
      .fromList(range.permutations.toList)
      .getOrElse(NonEmptyList.of(range))
      .map(x => Setting(x.toList))
  }

  final case class Output(state: MVar[IO, Int])
  def maxThrusterSignal(from: Int, to: Int, code: Array[Int])(
      implicit cs: ContextShift[IO]
  ): IO[(Int, Setting)] =
    possiblePhaseSettings(from, to)
      .parTraverse(
        setting =>
          for {
            mVar <- MVar.of[IO, Int](0)
            _ <- testSetting(Output(mVar), code, None, setting)
            result <- mVar.take
          } yield (result, setting)
      )
      .map(_.toList.maxBy(_._1))

  def testSetting(
      output: Output,
      code: Array[Int],
      prevOut: Option[Int],
      setting: Setting
  )(implicit cs: ContextShift[IO]): IO[Unit] = setting.value match {
    case Nil => IO.unit
    case head :: tl =>
      MVar
        .of[IO, Int](head)
        .flatMap(
          inputState =>
            TESTIntCode.diagnostic(
              onInput(inputState, prevOut),
              onOutput(output, code, Setting(tl), _),
              code
            )
        )
  }

  def onInput(inputState: MVar[IO, Int], prevOut: Option[Int]) =
    inputState.take.flatMap(x => inputState.put(prevOut.getOrElse(0)) >> IO(x))

  def onOutput(
      output: Output,
      code: Array[Int],
      setting: Setting,
      out: Int
  )(implicit cs: ContextShift[IO]): IO[Unit] =
    output.state.take >> output.state.put(out) >>
      testSetting(output, code, Some(out), setting)
}
