package example.models

import scala.util.chaining._
import scala.language.implicitConversions

import cats.Semigroup
import scala.math.Ordering
import cats.Foldable
import cats.implicits._
import cats.kernel.Monoid

trait Rucksackable[T] {
  def combineCompartments: T
}

final case class MapRucksack[K, V: Ordering: Semigroup](compartment1: Map[K, V], compartment2: Map[K, V]) extends Rucksackable[Map[K, V]] {
  def combineCompartments: Map[K, V] = compartment1 |+| compartment2

  def max: (K,V) = combineCompartments.maxBy{ case (k,v) => v }
}

final case class RucksackDups[V](key: Char, value: V)

trait MapRucksackSemigroup extends Semigroup[MapRucksack[Char,Int]] {
  override def combine(x: MapRucksack[Char, Int], y: MapRucksack[Char,Int]): MapRucksack[Char,Int] = {
    MapRucksack(x.compartment1 |+| y.compartment1, x.compartment2 |+| y.compartment2)
  }
}

object MapRucksack {
  implicit object mapRucksackSemigroup extends MapRucksackSemigroup

  def combineSacks(a: MapRucksack[Char, Int], b: MapRucksack[Char, Int], c: MapRucksack[Char, Int]): MapRucksack[Char, Int] = {
    a |+| b |+| c
  }

  def createCharIntMapRucksack(values: List[Char]): MapRucksack[Char, Int] = {
    val midpoint = values.length / 2
    val (front, back) = values.splitAt(midpoint)
    MapRucksack.apply[Char, Int] _ tupled charIntRucksackParams(front, back)
  }

  private[this] def charIntRucksackParams(front: List[Char], back: List[Char]): (Map[Char, Int], Map[Char, Int]) = {
    (
      front.foldLeft(Map[Char,Int]()) {(map, char) => map + (char -> 1)},
      back.foldLeft(Map[Char,Int]()) {(map, char) => map + (char -> 1)},
    )
  }
}
