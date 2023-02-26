package example.models

import cats.implicits._

// Warning. This is a mutable class :(
case class CrateStack(var values: List[Char] = List()) {
  def pop(): Option[Char] = {
    values match {
      case Nil => None
      case _ => {
        mutablePop()
      }
    }
  }

  def push(c: Char) = {
    values = c :: values
  }

  private[this] def mutablePop(): Option[Char] = {
    val h :: t = values
    values = t
    Some(h)
  }

}

object CrateStack {
  // This is just necessary for setup of initial state. Really I shouldn't
  // call this a stack because it's not really a true stack but meh it's just
  // a fun coding exercise and naming is time consuming for sillies.
  def addToStackBottom(cs: CrateStack, stackBottom: List[Char]): CrateStack = {
    CrateStack(cs.values |+| stackBottom)
  }

  def addToStackTop(cs: CrateStack, stackTop: List[Char]): CrateStack = {
    CrateStack(stackTop |+| cs.values)
  }
}
