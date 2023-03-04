package example
import cats.effect._
import example.fileio.Input

object Hello extends IOApp {
  override def run(args: List[String]): IO[ExitCode]  = {
    for {
      input <- if (args.length < 1) IO.raiseError(new IllegalArgumentException("Need to pass in a file")) else IO.pure(Input(args.head))
      values <- input.readFromSource()
      _ <- IO.println(values)
    } yield ExitCode.Success
  }


}
