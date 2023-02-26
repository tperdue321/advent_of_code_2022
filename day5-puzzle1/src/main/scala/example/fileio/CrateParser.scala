package example.fileio

import example.models.Action

object CrateParser {
  // This is disgusting but I hate string parsing and I'm not sure if I want to
  // learn a cleaner/better way right now just for a fun puzzle. I probably should though.
  def parseStackValues(line: String): List[List[Char]] = {
    val x = line.replaceAll("\\[|]", "").toList
    x.foldLeft(List[(Int, Char)]()) { (newList, char) =>
      if (newList.isEmpty && char.isSpaceChar) {
        newList :+ (0, '1')
      } else if (newList.isEmpty) {
        newList :+ (1, char)
      } else if (!newList.last._2.isDigit && char.isSpaceChar) {
        newList :+ (0, '1')
      } else if (char.isSpaceChar && newList.last._2.isDigit) {
        newList.updated(newList.length - 1, (((newList.last._2.asDigit + 1) / 4), f"${(newList.last._2.asDigit + 1)}".charAt(0)))
      } else {
        newList :+ (1, char)
      }
    }.flatMap {case (times, char) =>
      if (char.isDigit) {
        List.fill(times)(List())
      } else {
        List.fill(times)(List(char))
      }
    }
  }
  def parseAction(line: String): Action = {
    // There is not safety here.
    // knowingly writing in a lazy way. Would need to do validation and/or return
    // either a Try or Option if I care about safety/don't know my inputs are clean
    val s"move ${numPops} from ${from} to ${to}" = line
    Action(numPops.toInt, from.toInt - 1, to.toInt - 1)
  }
}
