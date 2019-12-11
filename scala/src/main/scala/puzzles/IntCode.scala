package puzzles
import puzzles.IntCode._
import cats.effect.IO
import puzzles.IntCode.Terminate._
import scala.util.Try
import puzzles.implicits._

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
      memory.getOrElse(index, 0)

    def values(): List[Code] =
      memory.toList.sortBy(_._1).map(_._2)

    def updated(index: Code, value: Code): Memory =
      Memory(memory.insertOrUpdate(index, value))

    /**
      * >>> import puzzles.IntCode._
      * >>> Memory.fromList(List(1,2,3,4)).drop(2)
      * Memory(Map(0 -> 3, 1 -> 4))
     **/
    def drop(index: Code): Memory =
      Memory(memory.collect {
        case (k, v) if (k >= index) => k - index -> v
      })
  }

  final case class Position(current: Code, base: Code = 0) {
    def +(by: Int) = Position(current + by, base)
    def toInt() = current.toInt
  }

  object Memory {
    def fromList(values: List[Code]) =
      Memory(
        values.zipWithIndex
          .map(_.swap.mapFst(_.toLong))
          .toMap
      )

    def parse(input: String): IO[Memory] =
      Util
        .readFile(input)
        .map(
          x =>
            fromList(
              x.getLines
                .flatMap(_.split(","))
                .map(_.toLong)
                .toList
            )
        )
  }

  sealed abstract class Op {
    final case class NextState private (pos: Position, xs: Memory)
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]]
    def next(pos: Position, xs: Memory) = Right(NextState(pos, xs))
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
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] =
      IO(
        next(pos + 4, if (pred(a(xs), b(xs))) {
          xs.updated(to.target, 1)
        } else {
          xs.updated(to.target, 0)
        })
      )
  }

  val cmpLessThan = Cmp({ case (x, y) => x < y }, _, _, _)
  val cmpEquals = Cmp({ case (x, y)   => x == y }, _, _, _)

  final case class Halt(e: Terminate) extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] =
      IO(Left(e))
  }
  val halt: Op = Halt(Terminate.End())
  def fail(e: Terminate) = Halt(e)

  final case class AdjustBase(a: Arg) extends Op {
    def exec(pos: Position, xs: Memory): IO[Either[Terminate, NextState]] =
      IO(next((pos + 2).copy(base = pos.base + a(xs)), xs))
  }

  sealed abstract class Arg extends Product {
    def apply(xs: Memory): Code
    def target(): Code
  }
  final case class Absolute(a: Code) extends Arg {
    def apply(xs: Memory): Code = xs(a)
    def target(): Code = a
  }
  final case class Relative(pos: Position, a: Code) extends Arg {
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
    * >>> arg(Position(0), 100, 42)
    * Immediate(42)
    *
    * >>> arg(Position(0), 102, 42)
    * Immediate(42)
    *
    * >>> arg(Position(0), 2, 42)
    * Absolute(42)
    *
    * >>> arg(Position(0), 10102, 42)
    * Immediate(42)
    *
    * >>> arg(Position(1), 10102, 42)
    * Absolute(42)
    *
    * >>> arg(Position(2), 10102, 42)
    * Immediate(42)
    *
    * >>> arg(Position(2), 102, 42)
    * Absolute(42)
    *
    * >>> arg(Position(0, 4), 202, 42)
    * Relative(Position(0,4),42)
   **/
  def arg(pos: Position, op: Code, a: Code): Arg =
    op.toString.dropRight((pos + 2).toInt).lastOption match {
      case Some('1') => Immediate(a)
      case Some('2') => Relative(pos, a)
      case _         => Absolute(a)
    }

  /**
    * >>> import cats.effect.IO
    * >>> import puzzles.IntCode._
    * >>> fromIntCode(IO(0), Position(0), Memory.fromList(List(1002,5,3,0,99,33)))
    * Mul(Absolute(5),Immediate(3),Absolute(0))
    *
    * >>> fromIntCode(IO(0), Position(0), Memory.fromList(List(1102, 2, -3, 4)))
    * Mul(Immediate(2),Immediate(-3),Absolute(4))
    *
    * >>> fromIntCode(IO(0), Position(0), Memory.fromList(List(1099, 2, 3, 4)))
    * Halt(End())
    *
    * >>> fromIntCode(IO(0), Position(1), Memory.fromList(List(42, 102, 2, 3, 4)))
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
        def arg_(x: Int) = arg(pos.copy(current = x), op, _)
        val (_1, _2, _3) = (arg_(0), arg_(1), arg_(2))
        (op % 100, args) match {
          case (1, a :: b :: to :: _) => Add(_1(a), _2(b), _3(to))
          case (2, a :: b :: to :: _) => Mul(_1(a), _2(b), _3(to))
          case (3, a :: _)            => Input(input, _1(a))
          case (4, a :: _)            => Halt(Output(_1(a)(xs), xs, pos + 2))
          case (5, a :: b :: _)       => jumpIfTrue(_1(a), _2(b))
          case (6, a :: b :: _)       => jumpIfFalse(_1(a), _2(b))
          case (7, a :: b :: to :: _) => cmpLessThan(_1(a), _2(b), _3(to))
          case (8, a :: b :: to :: _) => cmpEquals(_1(a), _2(b), _3(to))
          case (9, a :: _)            => AdjustBase(_1(a))
          case (99, _)                => halt
          case (op, _)                => fail(Terminate.UnknownOp(op.toString))
        }
    }
}
