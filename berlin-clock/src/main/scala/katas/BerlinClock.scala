package katas

object BerlinClock {
  def convertToBerlinTime(value: String): Array[String] = {
    // example: "13:17:01"
    val Array(hh, mm, ss) = value.split(":").map(_.toInt)

    Array(seconds(ss), topHours(hh), bottomHours(hh), topMinutes(mm), bottomMinutes(mm))
  }

  def bottomMinutes(value: Int): String = value % 5 match {
    case 0 => "OOOO"
    case 1 => "YOOO"
    case 2 => "YYOO"
    case 3 => "YYYO"
    case _ => "YYYY"
  }

  def topMinutes(value: Int): String = value / 5 match {
    case 0 => "OOOOOOOOOOO"
    case 1 => "YOOOOOOOOOO"
    case 2 => "YYOOOOOOOOO"
    case 3 => "YYROOOOOOOO"
    case 4 => "YYRYOOOOOOO"
    case 5 => "YYRYYOOOOOO"
    case 6 => "YYRYYROOOOO"
    case 7 => "YYRYYRYOOOO"
    case 8 => "YYRYYRYYOOO"
    case 9 => "YYRYYRYYROO"
    case 10 => "YYRYYRYYRYO"
    case _ => "YYRYYRYYRYY"
  }

  def bottomHours(value: Int) = value % 5 match {
    case 0 => "OOOO"
    case 1 => "ROOO"
    case 2 => "RROO"
    case 3 => "RRRO"
    case _ => "RRRR"
  }

  def topHours(value: Int): String = value / 5 match {
    case 0 => "OOOO"
    case 1 => "ROOO"
    case 2 => "RROO"
    case 3 => "RRRO"
    case _ => "RRRR"
  }

  def seconds(value: Int): String = value % 2 match {
    case 0 => "Y"
    case 1 => "O"
  }
}