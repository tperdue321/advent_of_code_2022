package example.fileio

import example.models.Packet

trait Reader[T] {
  def read(marker: T, packet: Packet): T
  def isStart(marker: T): Boolean
}
