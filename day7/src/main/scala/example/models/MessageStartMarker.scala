package example.models

final case class MessageStartMarker(packets: Vector[Packet], position: Int, dupLocation: Option[(Int, Int)]) extends StartMarker {
  override def head: Packet = packets.head
  override def tail: Vector[Packet] = packets.tail
}
