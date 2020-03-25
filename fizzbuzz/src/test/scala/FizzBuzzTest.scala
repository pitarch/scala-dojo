import org.scalatest._
import org.scalatest.Matchers._

// http://www.scalatest.org/user_guide/selecting_a_style
class FizzBuzzTest extends FunSpec {

  describe("Fizz Buzz") {
    List(3, 6, 9).foreach { input =>
      it(s"should return 'fizz' when divisible by 3 but not by 15: ${input}") {
        input % 3 shouldBe 0
        input % 15 shouldNot be(0)
        FizzBuzz.buzz(input).right.get shouldBe "fizz"
      }
    }

    List(5, 10).foreach { input =>
      it(s"should return 'buzz' when divisible by 5 but not by 15: ${input}") {
        input % 5 shouldBe 0
        FizzBuzz.buzz(input).right.get shouldBe "buzz"
      }
    }

    List(15, 30).foreach { input =>
      it(s"should return 'fizzbuzz' when divisible by 15: ${input}") {
        input % 15 shouldBe 0
        FizzBuzz.buzz(input).right.get shouldBe "fizzbuzz"
      }
    }

    List(1, 2, 4, 7).foreach { input =>
      it(s"should return $input when not divisible by neither 3 nor 5") {
        input % 3 shouldNot be(0)
        input % 5 shouldNot be(0)
        FizzBuzz.buzz(input).left.get shouldBe input
      }
    }

  }
}

object FizzBuzz {

  def buzz(number: Int): Either[Int, String] = {
    return (number % 3, number % 5) match {
      case (0, 0) => Right("fizzbuzz")
      case (_, 0) => Right("buzz")
      case (0, _) => Right("fizz")
      case _      => Left(number)
    }
  }
}
