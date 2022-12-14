package example.fileio

import scala.io.{BufferedSource, Source}
import cats.effect.{IO, Resource}
import cats.implicits._

import scala.annotation.tailrec
import scala.Predef._
import scala.util.Try
import example.models.MapRucksack

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

  def readFromSource(): IO[List[(Char, Int)]] = {
    inputSource().use { source =>
      IO.blocking(source.getLines()).map { iterator =>
        val h :: t = iterator.toList
        iterate(h, t,  ingest, List())
      }.handleErrorWith(_ => IO.pure(List()))
    }
  }

  @tailrec
  private[this] def iterate(value: String, input: List[String], 
                      ingest: (List[Char]) => (Char, Int), output: List[(Char, Int)] = List()
  ): List[(Char, Int)]  = {
    val appendedOutput = output :+ ingest(value.toCharArray().toList)
    input match  {
      case head :: tail => iterate(head, tail, ingest, appendedOutput)
      case Nil => appendedOutput
    }
  }

  def ingest(rucksack: List[Char]): (Char, Int) = {
    MapRucksack.createCharIntMapRucksack(rucksack).max
  }
}
