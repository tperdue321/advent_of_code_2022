package example.models

trait StartMarker {
  def head: Packet
  def tail: Vector[Packet]
  val position: Int
}
