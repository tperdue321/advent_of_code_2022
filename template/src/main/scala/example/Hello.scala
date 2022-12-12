package example
import cats.effect._
import example.fileio.Input

object Hello extends Greeting with IOApp {
  override def run(args: List[String]): IO[ExitCode]  = {
    for {
      input <- if (args.length < 1) IO.raiseError(new IllegalArgumentException("Need to pass in a file")) else IO.pure(Input(args.head))
      values <- input.readFromSource()
      _ <- IO.println(values)
      // example code from 2021 day 1
      // _ <- IO.println(increasingDepthCount(depths))
    } yield ExitCode.Success
  }


// example code from 2021 day 1
//   def increasingDepthCount(depths: List[Depth]): Int = {
//     depths.foldLeft(0) { (acc, depth) =>
//       depth.movement match {
//         case Some(Down()) => acc + 1
//         case _ => acc
//       }
//     }
//   }
}


trait Greeting {
  lazy val greeting: String = "hello"
}
