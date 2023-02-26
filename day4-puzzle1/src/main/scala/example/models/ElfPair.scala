package example.models

import cats.implicits.catsSyntaxSemigroup

case class Zoo(zebra: Int)
case class Foobar(baz: String)
case class ElfPair(elf1: Elf, elf2: Elf) {
  lazy val joinedSectionsByMinMax: Elf = elf1 |+| elf2
  def selfContainedPair: Boolean = {
    joinedSectionsByMinMax == elf1 || joinedSectionsByMinMax == elf2
  }

  def containingElf: Option[Elf] = {
    // lazy approach of just taking the semigroup
    if (selfContainedPair) {
      Some(joinedSectionsByMinMax)
    } else {
      None
    }
  }
}
