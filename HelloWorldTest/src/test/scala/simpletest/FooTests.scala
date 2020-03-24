package simpletest

import org.scalatest.FunSuite

class FooTests extends FunSuite {

  test("test") {

    val s1 = "1"
    val s2 = "2"
    val s3 = "foo"
    val y = for {
      a <- MyObject.toInt(s1)
      b <- MyObject.toInt(s2)
      c <- MyObject.toInt(s3)
    } yield a + b + c

    assert(y == Some(6))
  }
}

object MyObject {
  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    } catch {
      case e: Exception => None
    }
  }
}
