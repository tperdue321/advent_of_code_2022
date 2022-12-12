package example.models

import cats.Foldable
import cats.implicits._


final case class Game(opponent: Move, player: Move) extends AdventData {
  lazy val result: GameResult = player.play(opponent)

  lazy val score: Int = player.value + result.score
}

object Game {
  def createGame(opponent: String, player: String): Option[Game] = {
    for {
      op <- createMove(opponent)
      pl <- createMove(player)
    } yield Game(op, pl)
  }

  def createMove(move: String): Option[Move] = {
    move match {
      case "A" | "X" => Some(Rock)
      case "B" | "Y" => Some(Paper)
      case "C" | "Z" => Some(Scissors)
      case _ => None
    }
  }

  def totalScore(games: List[Game]): Int = {
    Foldable[List].foldMap(games)(_.score)
  }
}
