package example
import cats.effect._
import example.fileio.Input
import example.models.MapRucksack

object Hello extends Greeting with IOApp {
  override def run(args: List[String]): IO[ExitCode]  = {
    for {
      input <- if (args.length < 1) IO.raiseError(new IllegalArgumentException("Need to pass in a file")) else IO.pure(Input(args.head))
      values <- input.readFromSource()
      _ <- IO.println(ProcessValues.sumBadgeValues(values))
    } yield ExitCode.Success
  }
}

// If this wasn't just advent of code, I'd make a new datastructure, different util
// object, or other way to reason about this data, but this is fine for now even if it makes
// me cringe. I'd also reason about my IO better but oh well.
object ProcessValues {
  def calcBadges(sacks: List[MapRucksack[Char,Int]]): List[(Char, Int)] = {
    sacks.map { _.findByValue(3).get }
  }

  def sumBadgeValues(sacks: List[MapRucksack[Char,Int]]): Int = {
    calcBadges(sacks).foldLeft(0) {case (sum, (badge, _)) => sum + badgeValue(badge)}
  }

  def badgeValue(badge: Char): Int = if (badge.toInt % 96 > 26) badge.toInt % 64 + 26 else badge.toInt % 96
}


trait Greeting {
  lazy val greeting: String = "hello"
}
