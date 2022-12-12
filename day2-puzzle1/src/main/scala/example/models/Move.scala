package example.models

trait Move {
  def play(opponent: Move): GameResult
  val value: Int
}

final case object Rock extends Move {
  override def play(opponent: Move): GameResult = {
    opponent match {
      case Rock => Draw
      case Paper => Loss
      case Scissors => Win
    }
  }
  val value: Int = 1
}
final case object Paper extends Move {
  override def play(opponent: Move): GameResult = {
    opponent match {
      case Rock => Win
      case Paper => Draw
      case Scissors => Loss
    }
  }
  val value: Int = 2
}
final case object Scissors extends Move {
  override def play(opponent: Move): GameResult = {
    opponent match {
      case Rock => Loss
      case Paper => Win
      case Scissors => Draw
    }
  }
  val value: Int = 3
}

final case object NoMove extends Move {
  override def play(opponent: Move): GameResult = NotPlayed
  val value: Int = 0
}


