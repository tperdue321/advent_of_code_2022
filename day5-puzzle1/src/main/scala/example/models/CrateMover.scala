package example.models

import example.fileio.CrateParser

class CrateMover(val stacks: Vector[CrateStack] = CrateMover.defaultStack) {
  // this could be better. I'm getting lazy here.
  def applyAction(action: Action): CrateMover = {
    (stacks.lift(action.from), stacks.lift(action.to)) match {
      case (Some(from), Some(to)) => {
        val (stackRemoved, stackKept) = from.values.splitAt(action.numPops)
        val tmpStack = stacks.updated(action.from, CrateStack(stackKept))
        new CrateMover(tmpStack.updated(action.to, CrateStack.addToStackTop(to, stackRemoved)))
      }
      case _ => this
    }
  }
}

object CrateMover {

  // we know based on the puzzle input there are 9 stacks.
  // hard coding for now since there is no parameter to the puzzle
  // that requires a more elegant solution.
  // Might build these from a method but representing it this way
  // for now so my brain can visualize things.
  lazy val defaultStack: Vector[CrateStack] = Vector(
    CrateStack(),
    CrateStack(),
    CrateStack(),
    CrateStack(),
    CrateStack(),
    CrateStack(),
    CrateStack(),
    CrateStack(),
    CrateStack(),
  )

  // Take a list of values to be pushed onto the top of all stacks and do so
  def applyStackValues(crateMover: CrateMover = new CrateMover(), stackValues: List[List[Char]]): CrateMover = {

    val stacks = for {
      (stack, values) <- crateMover.stacks.zipAll(stackValues, CrateStack(), List())
    } yield CrateStack.addToStackBottom(stack, values)
    new CrateMover(stacks = stacks)
  }


  def execute(line: String, lastState: CrateMover): CrateMover = {
    // ignore lines with no chars, or the list of stacks
    if (line.contains('[')) {
      CrateMover.applyStackValues(lastState, CrateParser.parseStackValues(line))
    } else if (line.contains("move")) {
      lastState.applyAction(CrateParser.parseAction(line))
    } else {
      lastState
    }
  }
}