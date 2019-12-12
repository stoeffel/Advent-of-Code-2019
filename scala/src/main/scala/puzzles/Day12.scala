package puzzles
import cats.effect._
import cats.syntax.all._
import puzzles.implicits._
import atto._, Atto._
import atto.parser.character
import scala.collection.immutable.Nil
import scala.reflect.ManifestFactory

object Day12 extends AoCApp {
  def run()(implicit cs: ContextShift[IO]) =
    for {
      input <- Util.readFile("../input/day-12.txt")
      moons <- IO(
        input.getLines
          .flatMap(fromString _)
          .toList
      )
      result1 <- IO(simMotion(moons, 1000).map(_.energy).sum)
      result2 <- IO(historyRepeats(moons))
      _ <- Util.printStrLn(s"Part 1: $result1")
      _ <- Util.printStrLn(s"Part 2: $result2")
    } yield ()

  def simMotion(moons: List[Moon], steps: Int = 1): List[Moon] =
    if (steps == 0) {
      moons
    } else {
      simMotion(
        moons.map(_.motion(moons)).map(_.step),
        steps - 1
      )
    }

  def historyRepeats(t0: List[Moon]): Long = {
    val t1 = simMotion(t0)
    recurrenceDim(sameDim(_.x, t0), t1) lcm
      recurrenceDim(sameDim(_.y, t0), t1) lcm
      recurrenceDim(sameDim(_.z, t0), t1)
  }

  def recurrenceDim(
      pred: List[Moon] => Boolean,
      moons: List[Moon],
      steps: Long = 1
  ): Long =
    if (pred(moons)) {
      steps
    } else {
      recurrenceDim(pred, simMotion(moons), steps + 1)
    }

  def sameDim(from: Coords => Int, a: List[Moon])(b: List[Moon]): Boolean =
    a.map(x => from(x.position)) == b.map(x => from(x.position)) &&
      a.map(x => from(x.velocity)) == b.map(x => from(x.velocity))

  final case class Moon(
      position: Coords,
      velocity: Coords = Coords(0, 0, 0)
  ) {
    this: Moon =>

    /**
      * >>> import puzzles.Day12._
      * >>> Moon(Coords(1,2,3), Coords(-2, 2, -5)).step
      * Moon(Coords(-1,4,-2),Coords(-2,2,-5))
     **/
    def step = this.copy(position = position + velocity)

    def motion(others: List[Moon]): Moon =
      others match {
        case head :: tl =>
          influence(head).motion(tl)
        case Nil => this
      }

    /**
      * >>> import puzzles.Day12._
      * >>> Moon(Coords(1,2,3),Coords(-2,2,-5)).energy
      * 54
     **/
    def energy = kin * pot
    def pot = position.energy
    def kin = velocity.energy

    /**
      * >>> import puzzles.Day12._
      * >>> Moon(Coords(1,2,3),Coords(-2,2,-5)).influence(Moon(Coords(1,2,3),Coords(-2,2,-5)))
      * Moon(Coords(1,2,3),Coords(-2,2,-5))
      *
      * >>> Moon(Coords(1,2,3),Coords(-2,2,-5)).influence(Moon(Coords(2,1,4),Coords(-2,2,-5)))
      * Moon(Coords(1,2,3),Coords(-1,1,-4))
      *
      * >>> Moon(Coords(1,2,3),Coords(-2,2,-5)).influence(Moon(Coords(0,3,2),Coords(-2,2,-5)))
      * Moon(Coords(1,2,3),Coords(-3,3,-6))
     **/
    def influence(m: Moon): Moon =
      this.copy(
        velocity = Coords(
          x = velocity.x + (m.position.x compare position.x),
          y = velocity.y + (m.position.y compare position.y),
          z = velocity.z + (m.position.z compare position.z)
        )
      )
  }

  final case class Coords(x: Int, y: Int, z: Int) {
    def +(m: Coords) = Coords(x + m.x, y + m.y, z + m.z)

    def energy() = x.abs + y.abs + z.abs
  }

  /**
    * >>> Day12.fromString("<x=-7, y=-1, z=6>")
    * Some(Moon(Coords(-7,-1,6),Coords(0,0,0)))
   **/
  def fromString(input: String): Option[Moon] =
    positionParser
      .parseOnly(input)
      .option
      .map(Moon(_))

  private def positionParser: Parser[Coords] =
    (char('<') ~> sepBy(coordParser, char(',') ~ spaceChar) <~ char('>'))
      .flatMap {
        case List(x, y, z) => ok(Coords(x, y, z))
        case _             => err("Unknown Coords")
      }

  private def coordParser: Parser[Int] =
    oneOf("xyz") ~ char('=') ~> int
}
