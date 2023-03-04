package example.models

import cats.kernel.Eq

case class Packet(char: Char, position: Int)

trait PacketEq extends Eq[Packet] {
  override def eqv(x: Packet, y: Packet): Boolean = {
    x.char == y.char
  }
}
object Packet {
  implicit object packetEq extends PacketEq
  def apply(char: Char, marker: StartMarker): Packet = {
    new Packet(char, marker.position + 1)
  }
}