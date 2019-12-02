package puzzles
import puzzles.Op._

sealed abstract class Op extends Product {
  this: Op =>
  def run(xs: Array[Int]): Option[(Int, Array[Int])] = this match {
    case Terminate()   => None
    case Mul(a, b, to) => Some(3, xs.updated(to, xs(a) * xs(b)))
    case Add(a, b, to) => Some(3, xs.updated(to, xs(a) + xs(b)))
  }
}

object Op {
  final case class Add(a: Int, b: Int, to: Int) extends Op
  final case class Mul(a: Int, b: Int, to: Int) extends Op
  final case class Terminate() extends Op

  def fromIntCode(pos: Int, xs: Array[Int]): Either[Error, Op] =
    xs.drop(pos) match {
      case Array(1, a, b, to, _*) => Right(Add(a, b, to))
      case Array(2, a, b, to, _*) => Right(Mul(a, b, to))
      case Array(99, _*)          => Right(Terminate())
      case Array()                => Left(Error.EndOfInput())
      case Array(op, _*)          => Left(Error.UnknownOp(op))
    }

  sealed abstract class Error

  object Error {
    final case class UnknownOp(op: Int) extends Error
    final case class EndOfInput() extends Error
  }
}
