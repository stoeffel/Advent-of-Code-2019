package puzzles
import puzzles.Move.D
import puzzles.Move.U
import puzzles.Move.R
import puzzles.Move.L
import scala.util.parsing.combinator._

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

object Move extends RegexParsers {
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
    * >>> Move.fromString("L103,R123")
    * List(L(103), R(123))
    *
    * >>> Move.fromString("X103")
    * List()
    * }}}
   **/
  def fromString(input: String): List[Move] =
    parse(movesParser, input) match {
      case Success(matched, _) => matched
      case _                   => List()
    }

  private def movesParser: Parser[List[Move]] = rep1sep(move, ",".r)

  private def number: Parser[Int] = """(\d+)""".r.map(_.toInt)

  private def direction: Parser[String] = "(U|D|R|L){1}".r

  private def move: Parser[Move] = direction ~ number flatMap {
    case "U" ~ s => success(U(s))
    case "D" ~ s => success(D(s))
    case "L" ~ s => success(L(s))
    case "R" ~ s => success(R(s))
    case _       => failure("Unknown Move")
  }
}
