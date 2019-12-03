package puzzles
import puzzles.Move.D
import puzzles.Move.U
import puzzles.Move.R
import puzzles.Move.L

sealed abstract class Move extends Product {
  this: Move =>

  /**
    * {{{
    * >>> Move.D(4).steps((0,0))
    * List((0,-1), (0,-2), (0,-3), (0,-4))
    *
    * >>> Move.R(2).steps((1,2))
    * List((2,2), (3,2))
    * }}}
   **/
  def steps(from: (Int, Int)): List[(Int, Int)] = {
    val step = this match {
      case D(_) => (from._1, from._2 - 1)
      case U(_) => (from._1, from._2 + 1)
      case R(_) => (from._1 + 1, from._2)
      case L(_) => (from._1 - 1, from._2)
    }
    this.lower match {
      case None       => List()
      case Some(next) => step :: next.steps(step)
    }
  }

  private def lower(): Option[Move] = this match {
    case D(0) | U(0) | R(0) | L(0) => None
    case D(x)                      => Some(D(x - 1))
    case U(x)                      => Some(U(x - 1))
    case R(x)                      => Some(R(x - 1))
    case L(x)                      => Some(L(x - 1))
  }
}

object Move {
  final case class U(x: Int) extends Move
  final case class D(x: Int) extends Move
  final case class R(x: Int) extends Move
  final case class L(x: Int) extends Move

  /**
    * {{{
    * >>> Move.fromString("U1")
    * List(U(1))
    *
    * >>> Move.fromString("U2")
    * List(U(2))
    *
    * >>> Move.fromString("D234")
    * List(D(234))
    *
    * >>> Move.fromString("R1")
    * List(R(1))
    *
    * >>> Move.fromString("L103")
    * List(L(103))
    *
    * >>> import scala.util.Try
    * >>> Try(Move.fromString("X103")).toOption
    * None
    * }}}
   **/
  def fromString(input: String): List[Move] =
    input
      .split(",")
      .toList
      .map(getMove)

  private def getMove(input: String): Move =
    input.toSeq match {
      case Seq('U', rest @ _*) => U(rest.toString.toInt)
      case Seq('D', rest @ _*) => D(rest.toString.toInt)
      case Seq('R', rest @ _*) => R(rest.toString.toInt)
      case Seq('L', rest @ _*) => L(rest.toString.toInt)
      case _                   => throw (new Error("Unknown move"))
    }
}
