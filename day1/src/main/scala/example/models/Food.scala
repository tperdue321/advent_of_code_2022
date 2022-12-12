package example.models

import cats.kernel.Monoid

final case class Food(calories: Int)

object Food {
  implicit val foodMonoid: Monoid[Food] = new Monoid[Food] {
    def empty: Food = Food(0)
    def combine(x: Food, y: Food): Food = Food(x.calories + y.calories)
  }
}

