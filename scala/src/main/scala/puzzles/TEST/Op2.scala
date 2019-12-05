package puzzles
import puzzles.Op2._

sealed abstract class Op2 extends Product {
  type NextState = Option[(Int, Array[Int], Option[Int])]
  def exec(pos: Int, xs: Array[Int]): NextState
}

object Op2 {
  type Pred[T] = T => Boolean

  final case class Add(a: Arg, b: Arg, to: Int) extends Op2 {
    def exec(pos: Int, xs: Array[Int]): NextState =
      Some(pos + 4, xs.updated(to, a.value(xs) + b.value(xs)), None)
  }

  final case class Mul(a: Arg, b: Arg, to: Int) extends Op2 {
    def exec(pos: Int, xs: Array[Int]): NextState =
      Some(pos + 4, xs.updated(to, a.value(xs) * b.value(xs)), None)
  }

  final case class Input(a: Int, to: Int) extends Op2 {
    def exec(pos: Int, xs: Array[Int]): NextState =
      Some(pos + 2, xs.updated(to, a), None)
  }

  final case class Output(a: Arg) extends Op2 {
    def exec(pos: Int, xs: Array[Int]): NextState =
      Some(pos + 2, xs, Some(a.value(xs)))
  }

  final case class JumpIf(pred: Pred[Int], a: Arg, b: Arg) extends Op2 {
    def exec(pos: Int, xs: Array[Int]): NextState =
      if (pred(a.value(xs))) {
        Some(b.value(xs), xs, None)
      } else {
        Some(pos + 3, xs, None)
      }
  }
  val jumpIfTrue = JumpIf((_ != 0), _, _)
  val jumpIfFalse = JumpIf((_ == 0), _, _)

  final case class Cmp(pred: Pred[(Int, Int)], a: Arg, b: Arg, to: Int)
      extends Op2 {
    def exec(pos: Int, xs: Array[Int]): NextState = {
      val newXs = if (pred(a.value(xs), b.value(xs))) {
        xs.updated(to, 1)
      } else {
        xs.updated(to, 0)
      }
      Some(pos + 4, newXs, None)
    }
  }

  val cmpLessThan = Cmp({ case (x, y) => x < y }, _, _, _)
  val cmpEquals = Cmp({ case (x, y)   => x == y }, _, _, _)

  final case class Terminate() extends Op2 {
    def exec(pos: Int, xs: Array[Int]): NextState = None
  }

  sealed abstract class Arg extends Product {
    this: Arg =>
    def value(xs: Array[Int]): Int = this match {
      case Position(x)  => xs(x)
      case Immediate(x) => x
    }
  }
  final case class Position(a: Int) extends Arg
  final case class Immediate(a: Int) extends Arg

  /**
    * {{{
    * >>> Op2.arg(0, 100)
    * Immediate
    *
    * >>> Op2.arg(0, 102)
    * Immediate
    *
    * >>> Op2.arg(0, 2)
    * Position
    *
    * >>> Op2.arg(0, 10102)
    * Immediate
    *
    * >>> Op2.arg(1, 10102)
    * Position
    *
    * >>> Op2.arg(2, 10102)
    * Immediate
    *
    * >>> Op2.arg(2, 102)
    * Position
    * }}}
   **/
  def arg(pos: Int, code: Int): (Int => Arg) =
    code.toString.dropRight(2 + pos).lastOption match {
      case Some('1') => Immediate
      case _         => Position
    }

  /**
    * {{{
    *
    * >>> Op2.fromIntCode(0, 0, Array(1002,5,3,0,99,33))
    * Right(Mul(Position(5),Immediate(3),0))
    *
    * >>> Op2.fromIntCode(0, 0, Array(1102, 2, -3, 4))
    * Right(Mul(Immediate(2),Immediate(-3),4))
    *
    * >>> Op2.fromIntCode(0, 0, Array(1099, 2, 3, 4))
    * Right(Terminate())
    *
    * >>> Op2.fromIntCode(0, 1, Array(42, 102, 2, 3, 4))
    * Right(Mul(Immediate(2),Position(3),4))
    *
    * }}}
    **/
  def fromIntCode(input: Int, pos: Int, xs: Array[Int]): Either[Error, Op2] =
    xs.drop(pos) match {
      case Array() => Left(Error.EndOfInput())
      case Array(op, args @ _*) =>
        val a0 = arg(0, op)
        val a1 = arg(1, op)
        (op % 100, args) match {
          case (1, Seq(a, b, to, _*)) => Right(Add(a0(a), a1(b), to))
          case (2, Seq(a, b, to, _*)) => Right(Mul(a0(a), a1(b), to))
          case (3, Seq(a, _*))        => Right(Input(input, a))
          case (4, Seq(a, _*))        => Right(Output(a0(a)))
          case (5, Seq(a, b, _*))     => Right(jumpIfTrue(a0(a), a1(b)))
          case (6, Seq(a, b, _*))     => Right(jumpIfFalse(a0(a), a1(b)))
          case (7, Seq(a, b, to, _*)) => Right(cmpLessThan(a0(a), a1(b), to))
          case (8, Seq(a, b, to, _*)) => Right(cmpEquals(a0(a), a1(b), to))
          case (99, _)                => Right(Terminate())
          case (op, _)                => Left(Error.UnknownOp(op))
        }
    }

  sealed abstract class Error

  object Error {
    final case class UnknownOp(op: Int) extends Error
    final case class EndOfInput() extends Error
  }
}
