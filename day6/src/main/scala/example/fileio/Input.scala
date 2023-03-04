package example.fileio

import scala.io.{BufferedSource, Source}
import cats.effect.{IO, Resource}
import cats.implicits._

import scala.annotation.tailrec
import scala.util.Either
import scala.util.Try
import example.models.{Packet, PacketStartMarker}
import example.models.MessageStartMarker


// case class Input(filePath: String, reader: Reader[PacketStartMarker] = new PacketStartReader) {
case class Input(filePath: String, reader: Reader[MessageStartMarker] = new MessageStartReader) {
  type StartFound = Boolean

  def inputSource(): Resource[IO, BufferedSource] = {
    Resource.make {
      IO.blocking(Source.fromFile(filePath))
    } { source =>
      IO.blocking(source.close()).handleErrorWith{ _ =>
        IO.unit
      }
    }
  }

  // def readFromSource(): IO[PacketStartMarker] = {
  def readFromSource(): IO[MessageStartMarker] = {
    inputSource().use { source =>
      IO.blocking(source.getLines()).map { iterator =>
        // iterator.foldLeft(PacketStartMarker(Vector(), 0, None)) { (startMarker, nextLine) =>
        iterator.foldLeft(MessageStartMarker(Vector(), 0, None)) { (startMarker, nextLine) =>
          val input = LazyList.from(nextLine)
//
          input.foldM(startMarker){
            case (m, nextChar) => {
              // val nextMarker = reader.read(m, Packet[PacketStartMarker](nextChar, m))
              val nextMarker = reader.read(m, Packet(nextChar, m))
              if (reader.isStart(nextMarker)) {
                Either.left(nextMarker)
              } else {
                Either.right(nextMarker)
              }
            }
          }.merge
        }
      }.handleErrorWith(e => {
        println(e)
        // IO.pure(PacketStartMarker(Vector(), 0, None))
        IO.pure(MessageStartMarker(Vector(), 0, None))
      })
    }
  }



//  @tailrec
//  private[this] def iterate(nextVal: Char, input: LazyList[Char],
//                      ingest: (List[Char]) => (Char, Int), output: List[(Char, Int)] = List()
//  ): List[(Char, Int)]  = {
//    val appendedOutput = output :+ ingest(value.toCharArray().toList)
//    input match  {
//      case head :: tail => iterate(head, tail, ingest, appendedOutput)
//      case Nil => appendedOutput
//    }
//  }
}
