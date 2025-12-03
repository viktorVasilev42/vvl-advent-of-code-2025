package day1

case class SafeInstruction(direction: Char, strValue: String):
  def getIntValue: Option[Int] = this match
    case SafeInstruction('L', stringVal) => Some(-stringVal.toInt)
    case SafeInstruction('R', stringVal) => Some(stringVal.toInt)
    case _ => None
