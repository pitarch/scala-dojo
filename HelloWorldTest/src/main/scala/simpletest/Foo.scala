package simpletest

object Foo {
  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s.trim))
    } catch {
      case e: Exception => None
    }
  }

  def useToInt() {
    toInt("aa") match {
      case Some(i) => println(i)
      case None    => println("That didn't work.")
    }
  }
}
