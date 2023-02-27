package example.fileio

trait InputValue {
  
}

final case class ValuePresent(value: String) extends InputValue
final case class ValueMissing() extends InputValue
