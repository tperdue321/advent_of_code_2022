package example.models

case class PacketStartMarker(packets: Vector[Packet], position: Int, dupLocation: Option[(Int, Int)]) {
}

object PacketStartMarker {
//  def apply(packets: Vector[Packet]):
}
