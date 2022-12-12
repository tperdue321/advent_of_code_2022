package example.fileio

import example.models.{AdventData, Game}

import scala.Predef.String
import scala.io.{BufferedSource, Source}
import cats.effect.{IO, Resource}
import cats.implicits._

import scala.Predef._
import scala.annotation.tailrec
import scala.Array

case class Input(filePath: String) {

  def inputSource(): Resource[IO, BufferedSource] = {
    Resource.make {
      IO.blocking(Source.fromFile(filePath))
    } { source =>
      IO.blocking(source.close()).handleErrorWith(_ => IO.unit)
    }
  }

  def readFromSource(): IO[List[Game]] = {
    inputSource().use { source =>
      IO.blocking(source.getLines()).map { iterator =>
        val h :: t = iterator.toList
        iterate(h, t, Game.createGame, List())
      }.handleErrorWith(_ => IO.pure(List()))
    }
  }

  @tailrec
  private def iterate(value: String, input: List[String], ingest: (String, String) => Option[Game], output: List[Game] = List()): List[Game]  = {
    val newGame = value.split(" ").toList.take(2) match {
      case List(opponent, player) => ingest(opponent, player)
      case _ => None
    }
    input match  {
      case head :: tail => iterate(head, tail, ingest, output ++ newGame)
      case Nil => output ++ newGame
    }
  }
}
