package puzzles
import scala.collection.immutable.Nil
import puzzles.implicits._

object Password {
  type Validation[T] = T => Boolean

  def possibilities(extra: Validation[String])(from: Int, to: Int): Int =
    Range(from, to).count(valid(extra))

  def valid(extra: Validation[String]): Validation[Int] =
    x => increasingNumbers(x.toString) && extra(x.toString)

  /**
    * {{{
    * >>> Password.increasingNumbers("12345")
    * true
    *
    * >>> Password.increasingNumbers("12245")
    * true
    *
    * >>> Password.increasingNumbers("12143")
    * false
    * }}}
   **/
  val increasingNumbers: Validation[String] =
    x => x.toSeq.sorted == x.toSeq

  /**
    * {{{
    * >>> Password.hasDouble1("12345")
    * false
    *
    * >>> Password.hasDouble1("12245")
    * true
    *
    * >>> Password.hasDouble1("12143")
    * false
    *
    * >>> Password.hasDouble1("1211143")
    * true
    * }}}
   **/
  val hasDouble1: Validation[String] =
    _.toSeq.group.any(_.length >= 2)

  /**
    * {{{
    * >>> Password.hasDouble2("12345")
    * false
    *
    * >>> Password.hasDouble2("12245")
    * true
    *
    * >>> Password.hasDouble2("12143")
    * false
    *
    * >>> Password.hasDouble2("1211143")
    * false
    *
    * >>> Password.hasDouble2("12111443")
    * true
    * }}}
   **/
  val hasDouble2: Validation[String] =
    _.toSeq.group.any(_.length == 2)
}
