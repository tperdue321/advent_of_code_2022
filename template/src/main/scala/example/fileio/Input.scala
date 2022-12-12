package example.fileio

import example.models.{AdventData, PlaceHolder}

import scala.Predef.String
import scala.io.{BufferedSource, Source}
import cats.effect.{IO, Resource}
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.util.Try

case class Input(filePath: String) {

  def inputSource(): Resource[IO, BufferedSource] = {
    Resource.make {
      IO.blocking(Source.fromFile(filePath))
    } { source =>
      IO.blocking(source.close()).handleErrorWith(_ => IO.unit)
    }
  }

  def readFromSource(): IO[List[AdventData]] = {
    inputSource().use { source =>
      IO.blocking(source.getLines()).map { iterator =>
        val h :: t = iterator.toList
        iterate(h, t, None, List())
      }.handleErrorWith(_ => IO.pure(List[PlaceHolder]("borked")))
    }
  }

  @tailrec
  private def iterate(value: String, input: List[String], prevVal: Option[AdventData], output: List[AdventData] = List()): List[AdventData]  = {
    input match  {
      case head :: tail => iterate(head, tail, Some(PlaceHolder(head)), output ++ Seq(PlaceHolder(head)))
      case Nil => output ++ Seq(PlaceHolder())
    }
  }
}
