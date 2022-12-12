package example.models

trait Move {
  def play(opponent: Move): GameResult
  val value: Int
  val beats: Move
  val loses: Move
}

final case object Rock extends Move {
  override val beats: Move = Scissors
  override val loses: Move = Paper

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
  override val beats: Move = Rock
  override val loses: Move = Scissors
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
  override val beats: Move = Paper
  override val loses: Move = Rock
  override def play(opponent: Move): GameResult = {
    opponent match {
      case Rock => Loss
      case Paper => Win
      case Scissors => Draw
    }
  }
  val value: Int = 3
}
