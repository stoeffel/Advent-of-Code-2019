package puzzles
import puzzles.Op._
import cats.effect.IO
import puzzles.Op.Terminate._

sealed abstract class Op {
  final case class NextState(pos: Int, xs: Array[Int])
  def exec(pos: Int, xs: Array[Int]): IO[Either[Terminate, NextState]]
  def next(pos: Int, xs: Array[Int]) =
    Right(NextState(pos, xs))
}

object Op {
  type Pred[T] = T => Boolean
  final case class Add(a: Arg, b: Arg, to: Int) extends Op {
    def exec(pos: Int, xs: Array[Int]): IO[Either[Terminate, NextState]] =
      IO { next(pos + 4, xs.updated(to, a.value(xs) + b.value(xs))) }
  }

  final case class Mul(a: Arg, b: Arg, to: Int) extends Op {
    def exec(pos: Int, xs: Array[Int]): IO[Either[Terminate, NextState]] =
      IO { next(pos + 4, xs.updated(to, a.value(xs) * b.value(xs))) }
  }

  final case class Input(f: IO[Int], to: Int) extends Op {
    def exec(pos: Int, xs: Array[Int]): IO[Either[Terminate, NextState]] = {
      for {
        a <- f
      } yield next(pos + 2, xs.updated(to, a))
    }
  }

  final case class JumpIf(pred: Pred[Int], a: Arg, b: Arg) extends Op {
    def exec(pos: Int, xs: Array[Int]): IO[Either[Terminate, NextState]] =
      IO {
        if (pred(a.value(xs))) {
          next(b.value(xs), xs)
        } else {
          next(pos + 3, xs)
        }
      }
  }
  val jumpIfTrue = JumpIf((_ != 0), _, _)
  val jumpIfFalse = JumpIf((_ == 0), _, _)

  final case class Cmp(pred: Pred[(Int, Int)], a: Arg, b: Arg, to: Int)
      extends Op {
    def exec(pos: Int, xs: Array[Int]): IO[Either[Terminate, NextState]] = {
      val newXs = if (pred(a.value(xs), b.value(xs))) {
        xs.updated(to, 1)
      } else {
        xs.updated(to, 0)
      }
      IO { next(pos + 4, newXs) }
    }
  }

  val cmpLessThan = Cmp({ case (x, y) => x < y }, _, _, _)
  val cmpEquals = Cmp({ case (x, y)   => x == y }, _, _, _)

  final case class Halt(e: Terminate) extends Op {
    def exec(pos: Int, xs: Array[Int]): IO[Either[Terminate, NextState]] = IO {
      Left(e)
    }
  }
  val halt: Op = Halt(Terminate.End())
  def fail(e: Terminate) = Halt(e)

  sealed abstract class Arg extends Product {
    def value(xs: Array[Int]): Int
  }
  final case class Position(a: Int) extends Arg {
    def value(xs: Array[Int]): Int = xs(a)
  }
  final case class Immediate(a: Int) extends Arg {
    def value(xs: Array[Int]): Int = a
  }

  sealed abstract class Terminate

  object Terminate {
    final case class UnknownOp(op: Int) extends Terminate
    final case class Output(out: Int, xs: Array[Int], pos: Int)
        extends Terminate
    final case class End() extends Terminate
  }

  /**
    * >>> Op.arg(0, 100)
    * Immediate
    *
    * >>> Op.arg(0, 102)
    * Immediate
    *
    * >>> Op.arg(0, 2)
    * Position
    *
    * >>> Op.arg(0, 10102)
    * Immediate
    *
    * >>> Op.arg(1, 10102)
    * Position
    *
    * >>> Op.arg(2, 10102)
    * Immediate
    *
    * >>> Op.arg(2, 102)
    * Position
   **/
  def arg(pos: Int, code: Int): (Int => Arg) =
    code.toString.dropRight(2 + pos).lastOption match {
      case Some('1') => Immediate
      case _         => Position
    }

  /**
    * >>> import cats.effect.IO
    * >>> Op.fromIntCode(IO(0), 0, Array(1002,5,3,0,99,33))
    * Mul(Position(5),Immediate(3),0)
    *
    * >>> Op.fromIntCode(IO(0), 0, Array(1102, 2, -3, 4))
    * Mul(Immediate(2),Immediate(-3),4)
    *
    * >>> Op.fromIntCode(IO(0), 0, Array(1099, 2, 3, 4))
    * Halt(End())
    *
    * >>> Op.fromIntCode(IO(0), 1, Array(42, 102, 2, 3, 4))
    * Mul(Immediate(2),Position(3),4)
    **/
  def fromIntCode(
      input: IO[Int],
      pos: Int,
      xs: Array[Int]
  ): Op =
    xs.drop(pos) match {
      case Array() => halt
      case Array(op, args @ _*) =>
        val a0 = arg(0, op)
        val a1 = arg(1, op)
        (op % 100, args) match {
          case (1, Seq(a, b, to, _*)) => Add(a0(a), a1(b), to)
          case (2, Seq(a, b, to, _*)) => Mul(a0(a), a1(b), to)
          case (3, Seq(a, _*))        => Input(input, a)
          case (4, Seq(a, _*))        => Halt(Output(a0(a).value(xs), xs, pos + 2))
          case (5, Seq(a, b, _*))     => jumpIfTrue(a0(a), a1(b))
          case (6, Seq(a, b, _*))     => jumpIfFalse(a0(a), a1(b))
          case (7, Seq(a, b, to, _*)) => cmpLessThan(a0(a), a1(b), to)
          case (8, Seq(a, b, to, _*)) => cmpEquals(a0(a), a1(b), to)
          case (99, _)                => halt
          case (op, _)                => fail(Terminate.UnknownOp(op))
        }
    }
}
