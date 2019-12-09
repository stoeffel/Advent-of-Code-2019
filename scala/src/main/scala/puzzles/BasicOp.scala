package puzzles
import puzzles.BasicOp._

sealed abstract class BasicOp extends Product {
  this: BasicOp =>
  def exec(xs: Array[Int]): Option[(Int, Array[Int])] = this match {
    case Terminate()   => None
    case Mul(a, b, to) => Some(3, xs.updated(to, xs(a) * xs(b)))
    case Add(a, b, to) => Some(3, xs.updated(to, xs(a) + xs(b)))
  }
}

object BasicOp {
  final case class Add(a: Int, b: Int, to: Int) extends BasicOp
  final case class Mul(a: Int, b: Int, to: Int) extends BasicOp
  final case class Terminate() extends BasicOp

  /**
    * >>> BasicOp.fromIntCode(0, Array(1, 2, 3, 4))
    * Right(Add(2,3,4))
    *
    * >>> BasicOp.fromIntCode(0, Array(2, 2, 3, 4))
    * Right(Mul(2,3,4))
    *
    * >>> BasicOp.fromIntCode(0, Array(99, 2, 3, 4))
    * Right(Terminate())
    *
    * >>> BasicOp.fromIntCode(1, Array(42, 2, 2, 3, 4))
    * Right(Mul(2,3,4))
    **/
  def fromIntCode(pos: Int, xs: Array[Int]): Either[Error, BasicOp] =
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
