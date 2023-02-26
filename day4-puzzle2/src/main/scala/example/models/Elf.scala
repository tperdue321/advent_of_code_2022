package example.models

import cats.Semigroup

case class Elf(sections: Range) {
  def overlaps(otherElf: Elf): Boolean = {
    sections.head <= otherElf.sections.last && sections.last >= otherElf.sections.head
  }
}

trait ElfSemiGroup extends Semigroup[Elf] {
  // creates the largest possible range from the two elves sections to clean up
  // creates the largest possible range from the two elves sections to clean up
  override def combine(elf1: Elf, elf2: Elf): Elf = {
    Elf(elf1.sections.head.min(elf2.sections.head) to elf1.sections.last.max(elf2.sections.last))
  }
}

object Elf {
  implicit object elfSemiGroup extends ElfSemiGroup
}
