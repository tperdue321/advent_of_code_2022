package example.fileio

import example.models.{Packet, PacketStartMarker}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

// import example.fileio.{PacketReader, PacketReaderImpl}
class PacketReaderSpec extends AnyFunSpec with Matchers {
  // val defaultPackets: Vector[Packet] = Vector(Packet('a', 1),Packet('b', 2),Packet('c', 3),Packet('d', 4))
  // val newPacket: Packet = Packet('e', 5)
  // val defaultPacketMarker = PacketStartMarker(defaultPackets, 4, None)
  // val reader: PacketReader = new PacketReaderImpl
  // describe("PacketReader") {
  //   describe("read") {
  //     val expectedPacketMarker = PacketStartMarker(Vector(Packet('e', 5),Packet('a', 1),Packet('b', 2),Packet('c', 3)), 5, None)
  //     it("should remove oldest packet and add new packet") {
  //       reader.read(defaultPacketMarker, newPacket) should equal(expectedPacketMarker)
  //     }
  //   }
  // }
}
