package example.models

import cats.Semigroup

case class Elf(sections: Range)

trait ElfSemiGroup extends Semigroup[Elf] {
  // creates the largest possible range from the two elfs sections to clean up
  override def combine(elf1: Elf, elf2: Elf): Elf = {
    Elf(elf1.sections.head.min(elf2.sections.head) to elf1.sections.last.max(elf2.sections.last))
  }
}

object Elf {
  implicit object elfSemiGroup extends ElfSemiGroup
}
