package puzzles
import cats.data.NonEmptyList
import scala.collection.immutable.Nil

object implicits {
  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  implicit class Transposable[T](xss: List[List[T]]) {
    /**
      * >>> import puzzles.implicits._
      * >>> List(List(1,2,3), List(4,5,6), List(7,8,9)).safeTranspose
      * List(List(Some(1), Some(4), Some(7)), List(Some(2), Some(5), Some(8)), List(Some(3), Some(6), Some(9)))
      *
      * >>> List(List(1,2,3), List(4,5), List(7)).safeTranspose
      * List(List(Some(1), Some(4), Some(7)), List(Some(2), Some(5), None), List(Some(3), None, None))
     **/
    def safeTranspose() =
      xss
        .map(_.map(Some(_)))
        .map(_.padTo(xss.maxBy(_.size).size, None))
        .transpose
  }

  implicit class ListTuple[T](val original: Iterator[T]) {
    def exactlyTwo(): (T, T) =
      original.toList match {
        case a :: b :: _ => (a, b)
        case _           => throw (new Error("too many elements"))
      }
  }

  implicit class InsertOrUpdate[K, V](val x: Map[K, V]) {
    def insertOrUpdate(k: K, v: V) =
      x.get(k) match {
        case None    => x ++ List(k -> v)
        case Some(_) => x.updated(k, v)
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

  implicit class RenderGrid[T](val xs: Traversable[Traversable[T]]) {
    def renderGrid(): String =
      xs.map(_.toList.mkString(""))
        .toList
        .mkString("\n")
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
