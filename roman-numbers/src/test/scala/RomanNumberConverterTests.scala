import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RomanNumberConverterTests extends AnyFlatSpec with Matchers {

  Seq(
    "I" -> 1,
    "IV" -> 4,
    "VI" -> 6,
    "VIII" -> 8,
    "IX" -> 9,
    "XIII" -> 13,
    "XVII" -> 17,
    "XIX" -> 19,
    "LXXXIII" -> 83,
    "XCIII" -> 93,
    "MCMXIX" -> 1919
  ) foreach { fixture =>
    fixture._1 should s"be converted into ${fixture._2}" in {
      Right(fixture._2) shouldBe RomanNumberConverter.convert(fixture._1)
    }
  }
}
