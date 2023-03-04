package example.models

case class PacketStartMarker(packets: Vector[Packet], position: Int, dupLocation: Option[(Int, Int)]) {
  def head: Packet = packets.head
  def tail: Vector[Packet] = packets.tail
}

