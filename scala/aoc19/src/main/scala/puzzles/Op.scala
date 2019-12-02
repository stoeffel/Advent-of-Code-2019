package puzzles
import puzzles.Op._

sealed abstract class Op extends Product {
  this: Op =>
  def run(xs: Array[Int]): Option[Array[Int]] = this match {
    case Terminate()        => None
    case Mul(in0, in1, out) => Some(xs.updated(out, xs(in0) * xs(in1)))
    case Add(in0, in1, out) => Some(xs.updated(out, xs(in0) + xs(in1)))
  }
}

object Op {
  final case class Add(in0: Int, in1: Int, out: Int) extends Op
  final case class Mul(in0: Int, in1: Int, out: Int) extends Op
  final case class Terminate() extends Op

  def fromIntCode(pos: Int, xs: Array[Int]): Either[Error, Op] =
    xs.drop(pos) match {
      case Array(1, in0, in1, out, _*) => Right(Add(in0, in1, out))
      case Array(2, in0, in1, out, _*) => Right(Mul(in0, in1, out))
      case Array(99, _*)               => Right(Terminate())
      case Array()                     => Left(Error.EndOfInput())
      case Array(op, _*)               => Left(Error.UnknownOp(op))
    }

  sealed abstract class Error

  object Error {
    final case class UnknownOp(op: Int) extends Error
    final case class EndOfInput() extends Error
  }
}
