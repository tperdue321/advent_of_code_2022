package example.models

import example.fileio.InputValue
import example.fileio.ValueMissing
import example.fileio.ValuePresent

import cats.Functor
import cats.Foldable
import cats.kernel.Monoid
import Food.foodMonoid
import cats.implicits._

final case class Elf(heldFood: List[Food] = List()) extends AdventData {
  def calcFood(): Int = {
    Foldable[List].fold(heldFood).calories
  }

}

object Elf {
  def parseStream(inputValue: Option[Int], maybeExistingElf: Option[Elf]): Option[Elf] = {
    (maybeExistingElf) match {
      case Some(elf) => {
        Functor[Option].lift(Food.apply).apply(inputValue).map { food =>
          elf.copy(heldFood = elf.heldFood :+ food)
        }
      }
      case None => {
        Functor[Option].lift(Food.apply).apply(inputValue).map { food =>
          Elf(List(food))
        }
      }
    }
  }

  def maxElf(elfs: List[Elf]): Option[Elf] = {
    Foldable[List].maximumByOption(elfs) {elf => elf.calcFood()}
  }

  def maxThreeElves(elfs: List[Elf]): List[Elf] = {
      elfs.sortBy(_.calcFood()).takeRight(3)
  }
}
