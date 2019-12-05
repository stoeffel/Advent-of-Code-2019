package puzzles

import scala.io.{Codec, BufferedSource, Source}
import java.io.File
import cats.effect.IO

object Util {
  def printStrLn(msg: String): IO[Unit] = IO(println(msg))
  def readFile(path: String)(implicit codec: Codec): IO[BufferedSource] =
    IO(Source.fromFile(path))
  def readLineIO(msg: String): IO[String] =
    IO(readLine(msg))
}
