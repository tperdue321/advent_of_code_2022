package example.models

import cats.Foldable
import cats.implicits._


final case class Game(opponent: Move, player: Move) extends AdventData {
  lazy val result: GameResult = player.play(opponent)

  lazy val score: Int = player.value + result.score
}

object Game {
  def createGame(opponent: String, playerChoice: String): Option[Game] = {
    for {
      op <- createOpponentMove(opponent)
      pl <- createPlayerMove(playerChoice, op)
    } yield Game(op, pl)
  }

  def createOpponentMove(move: String): Option[Move] = {
    move match {
      case "A" => Some(Rock)
      case "B" => Some(Paper)
      case "C" => Some(Scissors)
      case _ => None
    }
  }

  def createPlayerMove(playerChoice: String, op: Move): Option[Move] = {
    playerChoice match {
      case "X" => Some(op.beats)
      case "Y" => Some(op)
      case "Z" => Some(op.loses)
      case _ => None
    }
  }

  def totalScore(games: List[Game]): Int = {
    Foldable[List].foldMap(games)(_.score)
  }
}
