package puzzles
import puzzles.IntCode._
import cats.effect.IO
import puzzles.IntCode.Terminate._
import scala.util.Try

object IntCode {
  type Code = Long

  def parse(input: String): IO[Memory] = Memory.parse(input)

  def run(
      input: IO[Code],
      xs: Memory,
      pos: Position = Position(0)
  ): IO[List[Code]] =
    for {
      res <- diagnostic(input, xs, pos)
      out <- res match {
        case UnknownOp(e) => throw (new Error(s"Unknown op $e"))
        case End()        => IO(List())
        case Output(newOut, newCode, newPos) =>
          run(input, newCode, newPos)
            .map(xs => newOut +: xs)
      }
    } yield out

  /**
    * >>> import cats.effect.IO
    * >>> import puzzles.IntCode.Memory
    * >>> IntCode.diagnostic(IO(0), Memory.fromList(List(11002,5,3,0,99,33))).unsafeRunSync
    * End()
   **/
  def diagnostic(
      input: IO[Code],
      xs: Memory,
      pos: Position = Position(0)
  ): IO[Terminate] =
    fromIntCode(input, pos, xs)
      .exec(pos, xs)
      .flatMap(
        result =>
          result match {
            case Left(halt)  => IO(halt)
            case Right(next) => diagnostic(input, next.xs, next.pos)
          }
      )

  final case class Memory(memory: Map[Code, Code]) {
    def apply(index: Code): Code =
      if (memory.contains(index)) {
        memory(index)
      } else {
        0
      }

    def values(): List[Code] =
      memory.toList.sortBy(_._1).map(_._2)

    def updated(index: Code, value: Code): Memory =
      if (memory.contains(index)) {
        Memory(memory.updated(index, value))
      } else {
        Memory(memory ++ List((index, value)))
      }

    /**
      * >>> import puzzles.IntCode._
      * >>> Memory.fromList(List(1,2,3,4)).drop(2).values
      * List(3, 4)
     **/
    def drop(index: Code): Memory =
      Memory(memory.filter(_._1 >= index).map(x => x._1 - index -> x._2))
  }

  final case class Position(current: Code, base: Code = 0) {
    def +(by: Int) = Position(current + by, base)
    def toInt() = current.toInt
  }

  object Memory {
    def fromList(values: List[Code]) =
      Memory(
        values.zipWithIndex
          .map(x => x._2.toLong -> x._1)
          .toMap
      )

    def parse(input: String): IO[Memory] =
      Util
        .readFile(input)
        .map(
          _.getLines
            .flatMap(_.split(","))
            .flatMap { code =>
              Try(code.toLong).toOption
            }
            .toList
        )
        .map(Memory.fromList(_))
  }

  sealed abstract class Op {
    final case class NextState(pos: Position, xs: Memory)
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]]
    def next(pos: Position, xs: Memory) =
      Right(NextState(pos, xs))
  }

  type Pred[T] = T => Boolean
  final case class Add(a: Arg, b: Arg, to: Arg) extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] =
      IO { next(pos + 4, xs.updated(to.target, a(xs) + b(xs))) }
  }

  final case class Mul(a: Arg, b: Arg, to: Arg) extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] =
      IO { next(pos + 4, xs.updated(to.target, a(xs) * b(xs))) }
  }

  final case class Input(f: IO[Code], to: Arg) extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] = {
      for {
        a <- f
      } yield next(pos + 2, xs.updated(to.target, a))
    }
  }

  final case class JumpIf(pred: Pred[Code], a: Arg, b: Arg) extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] =
      IO {
        if (pred(a(xs))) {
          next(pos.copy(current = b(xs)), xs)
        } else {
          next(pos + 3, xs)
        }
      }
  }
  val jumpIfTrue = JumpIf((_ != 0), _, _)
  val jumpIfFalse = JumpIf((_ == 0), _, _)

  final case class Cmp(pred: Pred[(Code, Code)], a: Arg, b: Arg, to: Arg)
      extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] = {
      val newXs = if (pred(a(xs), b(xs))) {
        xs.updated(to.target, 1)
      } else {
        xs.updated(to.target, 0)
      }
      IO { next(pos + 4, newXs) }
    }
  }

  val cmpLessThan = Cmp({ case (x, y) => x < y }, _, _, _)
  val cmpEquals = Cmp({ case (x, y)   => x == y }, _, _, _)

  final case class Halt(e: Terminate) extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] = IO {
      Left(e)
    }
  }
  val halt: Op = Halt(Terminate.End())
  def fail(e: Terminate) = Halt(e)

  final case class AdjustBase(a: Arg) extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] =
      IO { next((pos + 2).copy(base = pos.base + a(xs)), xs) }
  }

  sealed abstract class Arg extends Product {
    def apply(xs: Memory): Code
    def target(): Code
  }
  final case class Absolute(a: Code) extends Arg {
    def apply(xs: Memory): Code = xs(a)
    def target(): Code = a
  }
  final case class Relative(pos: Position)(a: Code) extends Arg {
    def apply(xs: Memory): Code = xs(pos.base + a)
    def target(): Code = pos.base + a
  }
  final case class Immediate(a: Code) extends Arg {
    def apply(xs: Memory): Code = a
    def target(): Code = a
  }

  abstract class Terminate {
    def output(): Option[Code] = None
  }

  object Terminate {
    final case class UnknownOp(op: String) extends Terminate
    final case class Output(out: Code, xs: Memory, pos: Position)
        extends Terminate {
      override def output(): Option[Code] = Some(out)
    }
    final case class End() extends Terminate
  }

  /**
    * >>> import puzzles.IntCode._
    * >>> IntCode.arg(Position(0), 100)
    * Immediate
    *
    * >>> IntCode.arg(Position(0), 102)
    * Immediate
    *
    * >>> IntCode.arg(Position(0), 2)
    * Absolute
    *
    * >>> IntCode.arg(Position(0), 10102)
    * Immediate
    *
    * >>> IntCode.arg(Position(1), 10102)
    * Absolute
    *
    * >>> IntCode.arg(Position(2), 10102)
    * Immediate
    *
    * >>> IntCode.arg(Position(2), 102)
    * Absolute
    *
    * >>> IntCode.arg(Position(0, 4), 202)(42)
    * Relative(Position(0,4))
   **/
  def arg(pos: Position, code: Code): (Code => Arg) =
    code.toString.dropRight((pos + 2).toInt).lastOption match {
      case Some('1') => Immediate
      case Some('2') => Relative(pos)
      case _         => Absolute
    }

  /**
    * >>> import cats.effect.IO
    * >>> import puzzles.IntCode._
    * >>> IntCode.fromIntCode(IO(0), Position(0), Memory.fromList(List(1002,5,3,0,99,33)))
    * Mul(Absolute(5),Immediate(3),Absolute(0))
    *
    * >>> IntCode.fromIntCode(IO(0), Position(0), Memory.fromList(List(1102, 2, -3, 4)))
    * Mul(Immediate(2),Immediate(-3),Absolute(4))
    *
    * >>> IntCode.fromIntCode(IO(0), Position(0), Memory.fromList(List(1099, 2, 3, 4)))
    * Halt(End())
    *
    * >>> IntCode.fromIntCode(IO(0), Position(1), Memory.fromList(List(42, 102, 2, 3, 4)))
    * Mul(Immediate(2),Absolute(3),Absolute(4))
    **/
  def fromIntCode(
      input: IO[Code],
      pos: Position,
      xs: Memory
  ): Op =
    xs.drop(pos.current).values match {
      case Nil => halt
      case op :: args =>
        val a0 = arg(pos.copy(current = 0), op)
        val a1 = arg(pos.copy(current = 1), op)
        val a2 = arg(pos.copy(current = 2), op)
        (op % 100, args) match {
          case (1, a :: b :: to :: _) => Add(a0(a), a1(b), a2(to))
          case (2, a :: b :: to :: _) => Mul(a0(a), a1(b), a2(to))
          case (3, a :: _)            => Input(input, a0(a))
          case (4, a :: _)            => Halt(Output(a0(a)(xs), xs, pos + 2))
          case (5, a :: b :: _)       => jumpIfTrue(a0(a), a1(b))
          case (6, a :: b :: _)       => jumpIfFalse(a0(a), a1(b))
          case (7, a :: b :: to :: _) => cmpLessThan(a0(a), a1(b), a2(to))
          case (8, a :: b :: to :: _) => cmpEquals(a0(a), a1(b), a2(to))
          case (9, a :: _)            => AdjustBase(a0(a))
          case (99, _)                => halt
          case (op, _)                => fail(Terminate.UnknownOp(op.toString))
        }
    }
}
