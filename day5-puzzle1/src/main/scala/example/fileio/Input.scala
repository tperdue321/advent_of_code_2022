package example.fileio

import scala.io.{BufferedSource, Source}
import cats.effect.{IO, Resource}
import example.models.CrateMover

import java.io.{PrintWriter, StringWriter}
import scala.annotation.tailrec

case class Input(filePath: String) {

  def inputSource(): Resource[IO, BufferedSource] = {
    Resource.make {
      IO.blocking(Source.fromFile(filePath))
    } { source =>
      IO.blocking(source.close()).handleErrorWith{ _ =>
        IO.unit
      }
    }
  }

  def readFromSource(): IO[CrateMover] = {
    inputSource().use { source =>
      IO.blocking(source.getLines()).map { iterator =>
        val h :: t = iterator.toList
        iterate(h, t,  ingest, new CrateMover())
      }.handleErrorWith{ e =>
        val sw = new StringWriter()
        e.printStackTrace(new PrintWriter(sw))
        println(sw.toString)
        IO.pure(new CrateMover())
      }
    }
  }

  @tailrec
  private[this] def iterate(value: String, input: List[String],
                            ingest: (String, CrateMover) => CrateMover, lastState: CrateMover
  ): CrateMover  = {
    val nextState = ingest(value, lastState)
    input match  {
      case head :: tail => iterate(head, tail, ingest, nextState)
      case Nil => nextState
    }
  }
//
  def ingest(nextState: String, lastState: CrateMover): CrateMover = {
    CrateMover.execute(nextState, lastState)
  }
}
