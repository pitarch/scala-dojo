import RomanFigures._

sealed abstract class RomanFigure(val value: Int)

object RomanFigures {

  case object M extends RomanFigure(1000)

  case object D extends RomanFigure(500)

  case object C extends RomanFigure(100)

  case object L extends RomanFigure(50)

  case object X extends RomanFigure(10)

  case object V extends RomanFigure(5)

  case object I extends RomanFigure(1)

}

object RomanNumberConverter {

  def convert(roman: String): Either[Seq[String], Int] = {

    val input: Seq[Either[String, RomanFigure]] = roman.toCharArray.toSeq map { char =>
      val figure = convertToRomanFigure(char)
      figure match {
        case Some(value) => Right(value)
        case None => Left(s"Not a roman figure: '${char}'")
      }
    }

    val result = input partition {
      _.isLeft
    } match {
      case (Nil, xs) => Right(reduceRomanFigureSeq(xs map {
        _.right.get.value
      }))
      case (xs, _) => Left(xs map {
        _.left.get
      })
    }

    result
  }


  /**
    * reduce a sequence of arabic values to an integer
    *
    * @param seq integer seq consisting of having converted the roman figure to its arabic value. For instance,
    *            given the roman number 'MCM', the expected input will be Seq(1000, 100, 1000)
    * @return
    */
  def reduceRomanFigureSeq(seq: Seq[Int]): Int = (seq
    .zip {
      seq.tail
    } :+ (seq.last, 0))
    .map { case (l, r) => if (l < r) (-l, r) else (l, r) }
    .foldLeft(0) { (acc: Int, pair: (Int, Int)) => acc + pair._1 }


  def convertToRomanFigure(figure: Char): Option[RomanFigure] = {
    figure match {
      case 'M' => Some(M)
      case 'D' => Some(D)
      case 'C' => Some(C)
      case 'L' => Some(L)
      case 'X' => Some(X)
      case 'V' => Some(V)
      case 'I' => Some(I)
      case _ => None
    }
  }
}