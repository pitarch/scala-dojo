package example

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.math.BigInteger
import scala.collection.immutable.Stream.cons

class FooTests extends FlatSpec with Matchers {

  def repeatedParameterMethod(x: Int, y: String, z: Any*) = {
    "%d %ss can give you %s".format(x, y, z.mkString(", "))
  }

  "test" should "test" in {
    println(
      repeatedParameterMethod(
        3,
        "egg",
        List("a delicious sandwich", "protein", "high cholesterol"): _*
      )
    )
  }
}
