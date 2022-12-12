package example
import cats.effect._
import example.fileio.Input
import example.models.Elf

object Hello extends Greeting with IOApp {
  override def run(args: List[String]): IO[ExitCode]  = {
    for {
      input <- if (args.length < 1) IO.raiseError(new IllegalArgumentException("Need to pass in a file")) else IO.pure(Input(args.head))
      values <- input.readFromSource()
      _ <- IO.println(Elf.maxThreeElves(values).foldLeft(0) {(sum, elf) => sum + elf.calcFood})
    } yield ExitCode.Success
  }


}


trait Greeting {
  lazy val greeting: String = "hello"
}
