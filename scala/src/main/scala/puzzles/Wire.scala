package puzzles
import cats.effect.IO
import puzzles.implicits._

object Wire {
  type Point = (Int, Int)
  type Wire = List[Point]

  def parse(input: String): IO[(Wire, Wire)] =
    Util
      .readFile(input)
      .map(
        _.getLines
          .map(fromString)
          .exactlyTwo
      )

  /**
    * {{{
    * >>> Wire.closestIntersection(
    * ...   Wire.fromString("R75,D30,R83,U83,L12,D49,R71,U7,L72"),
    * ...   Wire.fromString("U62,R66,U55,R34,D71,R55,D58,R83")
    * ...  )
    * Some(159)
    *
    * >>> Wire.closestIntersection(
    * ...   Wire.fromString("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"),
    * ...   Wire.fromString("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    * ...  )
    * Some(135)
    * }}}
    **/
  def closestIntersection(ab: (Wire, Wire)): Option[Int] =
    intersections(ab._1, ab._2)
      .map(_.manhattan)
      .sorted
      .headOption

  /**
    * {{{
    * >>> Wire.leastSteps(
    * ...   Wire.fromString("R8,U5,L5,D3"),
    * ...   Wire.fromString("U7,R6,D4,L4")
    * ... )
    * Some(30)
    *
    * >>> Wire.leastSteps(
    * ...   Wire.fromString("R75,D30,R83,U83,L12,D49,R71,U7,L72"),
    * ...   Wire.fromString("U62,R66,U55,R34,D71,R55,D58,R83")
    * ... )
    * Some(610)
    *
    * >>> Wire.leastSteps(
    * ...   Wire.fromString("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"),
    * ...   Wire.fromString("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    * ... )
    * Some(410)
    *
    * }}}
   **/
  def leastSteps(ab: (Wire, Wire)): Option[Int] =
    intersections(ab._1, ab._2)
      .map(x => stepsTo(ab._1, x) + stepsTo(ab._2, x))
      .sorted
      .headOption

  /**
    * {{{
    * >>> Wire.stepsTo(
    * ...   Wire.fromString("R8,U5,L5,D3"),
    * ...   (6, 5))
    * 15
    * }}}
     **/
  def stepsTo(a: Wire, x: Point): Int =
    a.takeWhile(_ != x).length

  /**
    * {{{
    * >>> Wire.intersections(Wire.fromString("U2,R3,D4"),
    * ... Wire.fromString("R2,U4,L4"))
    * List((2,2))
    *
    * >>> Wire.intersections(Wire.fromString("U7,R6,D4,L4"),
    * ... Wire.fromString("R8,U5,L5,D3"))
    * List((6,5), (3,3))
    * }}}
    **/
  def intersections(a: Wire, b: Wire): List[Point] =
    a.intersect(b).filter(_ != (0, 0))

  /**
    * {{{
    * >>> Wire.fromString("U2")
    * List((0,0), (0,1), (0,2))
    *
    * >>> Wire.fromString("U2,R3")
    * List((0,0), (0,1), (0,2), (1,2), (2,2), (3,2))
    *
    * >>> Wire.fromString("U2,R3,D4")
    * List((0,0), (0,1), (0,2), (1,2), (2,2), (3,2), (3,1), (3,0), (3,-1), (3,-2))
    * }}}
   **/
  def fromString(raw: String): Wire =
    Move
      .fromString(raw)
      .foldLeft((0, 0), List((0, 0))) {
        case ((prev, acc), curr) => {
          val steps = curr.steps(prev)
          (steps.last, acc ++ steps)
        }
      }
      ._2
}
