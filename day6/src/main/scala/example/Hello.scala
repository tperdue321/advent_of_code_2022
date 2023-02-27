package example
import cats.effect._
import example.fileio.Input
import example.models.MapRucksack

object Hello extends Greeting with IOApp {
  override def run(args: List[String]): IO[ExitCode]  = {
    for {
      input <- if (args.length < 1) IO.raiseError(new IllegalArgumentException("Need to pass in a file")) else IO.pure(Input(args.head))
      values <- input.readFromSource()
      _ <- IO.println(values)
      _ <- IO.println(ProcessValues.calcPrioritySum(values))
    } yield ExitCode.Success
  }


}

// If this wasn't just advent of code, I'd make a new datastructure, different util
// object, or other way to reason about this data, but this is fine for now even if it makes
// me cringe.
object ProcessValues {
  def calcPrioritySum(dups: List[(Char, Int)]): Int = {
    dups.foldLeft(0) { case (sum, (c, i)) => sum + charValue(c) }
  }

  private[this] def charValue(c: Char): Int = if (c.toInt % 96 > 26) c.toInt % 64 + 26 else c.toInt % 96
}


trait Greeting {
  lazy val greeting: String = "hello"
}
