package puzzles
import cats.data.NonEmptyList

object implicits {
  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  implicit class ListTuple[T](val original: Iterator[T]) {
    def exactlyTwo(): (T, T) =
      original.toList match {
        case a :: b :: _ => (a, b)
        case _           => throw (new Error("too many elements"))
      }
  }

  implicit class Manhattan(val x: (Int, Int)) {
    def manhattan(): Int = x._1.abs + x._2.abs
  }

  implicit class MapTuple[A, B](val x: (A, B)) {
    def mapFst[C](f: A => C): (C, B) = f(x._1) -> x._2
    def mapSnd[C](f: B => C): (A, C) = x._1 -> f(x._2)
    def mapBoth[C, D](f: A => C, g: B => D): (C, D) = f(x._1) -> g(x._2)
  }

  implicit class GroupWhile[T](val xs: Traversable[T]) {
    def group(): List[NonEmptyList[T]] =
      xs.groupWhile((a, b) => a == b)

    def groupWhile(pred: (T, T) => Boolean): List[NonEmptyList[T]] =
      xs.foldRight(List[NonEmptyList[T]]())(
        (x, acc) =>
          acc match {
            case Nil => List(NonEmptyList.one(x))
            case (g :: groups) => {
              val y = g.head
              if (pred(x, y)) {
                g.append(x) :: groups
              } else {
                NonEmptyList.one(x) :: acc
              }
            }
          }
      )
  }

  implicit class AnyPred[T](val xs: Traversable[T]) {
    def any(pred: (T) => Boolean): Boolean =
      xs match {
        case Nil       => false
        case h :: rest => pred(h) || rest.any(pred)
      }
  }
}
