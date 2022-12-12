package example.fileio

import example.models.{AdventData, PlaceHolder, Elf}

import scala.Predef.String
import scala.io.{BufferedSource, Source}
import cats.effect.{IO, Resource}
import cats.implicits._

import scala.annotation.tailrec
import scala.Predef._
import scala.util.Try
// import cats.data.Nested
// import cats.implicits._

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

  def readFromSource(): IO[List[Elf]] = {
    inputSource().use { source =>
      IO.blocking(source.getLines()).map { iterator =>
        val h :: t = iterator.toList
        iterate(h, t, None, Elf.parseStream, List())
      }.handleErrorWith(_ => IO.pure(List()))
    }
  }

  @tailrec
  private[this] def iterate(value: String, input: List[String], prevElf: Option[Elf],
                      ingest: (Option[Int], Option[Elf]) => Option[Elf], output: List[Elf] = List()
  // ): List[Elf]  = {
  ): List[Elf]  = {
    // We are adding data to the prevElf if more data for the elf exists in the file
    // Otherwise we need to create a new elf and append the previous elf to the output
    val prevElfCopy = ingest(parseValue(value), prevElf)
    val updatedOutput = prevElfCopy match {
      case Some(value) => output
      case None => output ++ prevElf
    }
    input match  {
      case head :: tail => iterate(head, tail, prevElfCopy, ingest, updatedOutput)
      case Nil => updatedOutput
    }
  }

  private[this] def parseValue(value: String): Option[Int] = {
    Try(value.toInt).toOption
  } 
}
