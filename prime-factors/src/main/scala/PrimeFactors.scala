import scala.collection.mutable.ListBuffer

object PrimeFactors {

  def result(value: Int): List[Int] = {

    val primeFactors = new ListBuffer[Int]
    var base = value
    var counter = 2
    while (base >= counter) {
      if (base % counter == 0) {
        primeFactors += counter
        base = base / counter
      } else {
        counter += 1
      }
    }

    primeFactors.toList
  }
}