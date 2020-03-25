import org.scalatest.Matchers._
import org.scalatest._

object BerlinClock {}

class BerlinClockTestSuite extends FlatSpec with Matchers {

  // On the top of the clock there is a yellow lamp that blinks on/off every two seconds.
  "Berlin Clock" should "blink on/off every two seconds" {
    1 should not be 1
  }
}
