package example.fileio

import example.models.{Packet, PacketStartMarker}


object PacketReader {
  val PACKET_SIZE = 4
  def read(marker: PacketStartMarker, packet: Packet): PacketStartMarker = {
    PacketStartMarker(packet +: marker.packets.take(PACKET_SIZE - 1))
  }

  def checkStart(marker: PacketStartMarker): Boolean = {

    false
  }
}
