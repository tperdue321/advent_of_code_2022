package example.fileio

import cats.implicits.catsSyntaxEq
import cats.syntax.all._
import example.models.{Packet, PacketStartMarker}

import scala.util.chaining.scalaUtilChainingOps

trait PacketReader {

  def read(marker: PacketStartMarker, packet: Packet): PacketStartMarker
  def isStart(marker: PacketStartMarker): Boolean
}

class PacketReaderImpl extends PacketReader {
  val MARKER_SIZE = 4
  // Read the next packet into the packet start marker.
  // If have enough packets to be a valid marker, drop the oldest packet and read in new one.
  // Otherwise just read in the new packet.
  // create packet marker and check dup appropriately
  def read(marker: PacketStartMarker, packet: Packet): PacketStartMarker =
    PacketStartMarker(packet +: marker.packets.take(3), marker.position + 1, marker.dupLocation).pipe(processFullMarker)

  def isStart(marker: PacketStartMarker): Boolean = marker.dupLocation.isEmpty && marker.packets.size == MARKER_SIZE

  def processFullMarker(marker: PacketStartMarker): PacketStartMarker =
    marker.tail.find(p => p === marker.head) match {
      case Some(dup) => markDup(marker, Some(marker.head.position, dup.position))
      case None => checkDupInTail(marker)
    }

  def markDup(marker: PacketStartMarker, dup: Option[(Int, Int)]): PacketStartMarker =
    dup match {
      case Some(_) => marker.copy(dupLocation = List(marker.dupLocation, dup).flatten.maxByOption(_._2))
      case _ => marker.copy(dupLocation = None)
    }

  def checkDupInTail(marker: PacketStartMarker): PacketStartMarker =
    marker.dupLocation.map { dupPosition =>
      dupPosition.bimap[Boolean, Boolean](
        x => marker.tail.exists(_.position == x),
        y => marker.tail.exists(_.position == y),
      )
    } match {
      case Some((true, true)) => marker
      case _ => markDup(marker, None)
    }
}
