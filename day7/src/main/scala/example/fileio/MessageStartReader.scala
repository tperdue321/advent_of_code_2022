package example.fileio

import cats.implicits.catsSyntaxEq
import cats.syntax.all._

import example.models.{Packet, MessageStartMarker}
import scala.util.chaining.scalaUtilChainingOps


class MessageStartReader extends Reader[MessageStartMarker] {
  val MARKER_SIZE = 14
  // Read the next packet into the packet start marker.
  // If have enough packets to be a valid marker, drop the oldest packet and read in new one.
  // Otherwise just read in the new packet.
  // create packet marker and check dup appropriately
  def read(marker: MessageStartMarker, packet: Packet): MessageStartMarker =
    MessageStartMarker(packet +: marker.packets.take(MARKER_SIZE - 1), marker.position + 1, marker.dupLocation).pipe(processFullMarker)

  def isStart(marker: MessageStartMarker): Boolean = marker.dupLocation.isEmpty && marker.packets.size == MARKER_SIZE

  def processFullMarker(marker: MessageStartMarker): MessageStartMarker =
    marker.tail.find(p => p === marker.head) match {
      case Some(dup) => markDup(marker, Some(marker.head.position, dup.position))
      case None => checkDupInTail(marker)
    }

  def markDup(marker: MessageStartMarker, dup: Option[(Int, Int)]): MessageStartMarker =
    dup match {
      case Some(_) => marker.copy(dupLocation = List(marker.dupLocation, dup).flatten.maxByOption(_._2))
      case _ => marker.copy(dupLocation = None)
    }

  def checkDupInTail(marker: MessageStartMarker): MessageStartMarker =
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
