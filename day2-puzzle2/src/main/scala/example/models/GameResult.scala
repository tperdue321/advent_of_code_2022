package example.models 
trait GameResult {
  val score: Int
}

final case object Loss extends  GameResult {
  val score: Int = 0
}
final case object Draw extends  GameResult {
  val score: Int = 3
}
final case object Win extends  GameResult {
  val score: Int = 6
}
