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

  def readFromSource(): IO[List[MapRucksack[Char, Int]]] = {
    inputSource().use { source =>
      IO.blocking(source.getLines()).map { iterator =>
        val h :: t = iterator.toList
        iterate(h, t, List(), ingest, List())
      }.handleErrorWith{ _ => IO.pure(List()) }
    }
  }

  @tailrec
  // I'd like to fix all this[char,int] nonesense but maybe later for practice
  private[this] def iterate(
    value: String,
    input: List[String],
    prevSacks: List[MapRucksack[Char, Int]],
    ingest: (List[Char], List[MapRucksack[Char, Int]]) => Either[MapRucksack[Char, Int], List[MapRucksack[Char, Int]]],
    output: List[MapRucksack[Char, Int]]
  ): List[MapRucksack[Char, Int]]  = {
    val appendedOutput = output :+ ingest(value.toCharArray().toList, prevSacks)
    (ingest(value.toCharArray().toList, prevSacks), input) match {
      case (Left(value),  head :: tail) => iterate(head, tail, List(), ingest, output :+ value)
      case (Left(value),  Nil) => output :+ value
      case (Right(sacks), head :: tail) => iterate(head, tail, sacks, ingest, output)
      case (Right(sacks), Nil) => output
    }
  }

  def ingest(
    rucksack: List[Char], prevSacks: List[MapRucksack[Char, Int]]
  ): Either[MapRucksack[Char, Int], List[MapRucksack[Char, Int]]] = {
    val currentSax = MapRucksack.createCharIntMapRucksack(rucksack)
    
    val sax: List[MapRucksack[Char, Int]] = prevSacks :+ currentSax

    if (sax.length == 3) {
      val a :: b :: c :: tail = sax
      Left(MapRucksack.combineSacks(a, b, c))
    } else {
      Right(sax)
    }
  }

}
