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
  def read(marker: PacketStartMarker, packet: Packet): PacketStartMarker = {
    if (marker.packets.size == MARKER_SIZE) {
      // check dup and create packet marker appropriately
      PacketStartMarker(packet +: marker.packets.take(3), marker.position + 1, None)
    } else {
      PacketStartMarker(packet +: marker.packets, marker.position + 1, None)
    }
  }

  def isStart(marker: PacketStartMarker): Boolean = {
    !marker.packets.tail.contains(marker.packets.head) && marker.dupLocation.isEmpty && marker.packets.size == MARKER_SIZE
  }

  def processFullMarker(marker: PacketStartMarker, packet: Packet): PacketStartMarker = {
    marker.packets.find(p => p === packet) match {
      case Some(dup) => markDup(marker, Some(packet.position, dup.position))
      case None => markDup(marker, None)
    }
  }

  def markDup(marker: PacketStartMarker, dup: Option[(Int, Int)]): PacketStartMarker = {
    marker.copy(dupLocation = dup)
  }
  def processNextPacket(marker: PacketStartMarker, packet: Packet): PacketStartMarker = {
    marker.dupLocation.map { dupPosition =>
      dupPosition.bimap[Boolean, Boolean](
        x => marker.packets.exists(_.position == x),
        y => marker.packets.exists(_.position == y),
      )
    } match {
      case Some((true, true)) => marker
      case _ => markDup(marker, None)
    }
  }
}
