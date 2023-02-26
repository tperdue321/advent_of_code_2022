package example.fileio

import scala.io.{BufferedSource, Source}
import cats.effect.{IO, Resource}
import cats.implicits._

import scala.annotation.tailrec
import scala.Predef._
import scala.util.Try
import example.models.{Elf, ElfPair, MapRucksack}

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

  def readFromSource(): IO[List[Boolean]] = {
    inputSource().use { source =>
      IO.blocking(source.getLines()).map { iterator =>
        val h :: t = iterator.toList
        iterate(h, t,  ingest, List())
      }.handleErrorWith(_ => IO.pure(List()))
    }
  }

  @tailrec
  private[this] def iterate(value: String, input: List[String], 
                      ingest: (String) => Boolean, output: List[Boolean] = List()
  ): List[Boolean]  = {
    val appendedOutput = output :+ ingest(value)
    input match  {
      case head :: tail => iterate(head, tail, ingest, appendedOutput)
      case Nil => appendedOutput
    }
  }

  def ingest(elfPair: String): Boolean = {
    val pair = elfPair.split(',').map { sectionRange =>
      sectionRange.split('-') match {
        case Array(first, last) => Elf(first.toInt to last.toInt)
      }
    } match {
      case Array(elf1, elf2) => ElfPair(elf1, elf2)
    }

    pair.overlappingElves
  }

//  def ingest(rucksack: List[Char]): (Char, Int) = {
//    MapRucksack.createCharIntMapRucksack(rucksack).max
//  }
}
