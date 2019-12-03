package puzzles

object implicits {
  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  class ListTuple[T](val original: Iterator[T]) {
    def exactlyTwo(): (T, T) =
      original.toList match {
        case a :: b :: _ => (a, b)
        case _           => throw (new Error("too many elements"))
      }
  }

  implicit def implicitListTuple[T](value: Iterator[T]) =
    new ListTuple(value)

  class Manhattan(val x: (Int, Int)) {
    def manhattan(): Int = x._1.abs + x._2.abs
  }

  implicit def implicitManhattan(value: (Int, Int)) =
    new Manhattan(value)
}
