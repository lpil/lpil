object PerfectNumbers {
  def classify(n: Int): Either[String, NumberType] = {
    lazy val sumOfFactors = (1 until n).foldLeft(0) { (acc, i) =>
      if (n % i == 0) acc + i else acc
    }
    if (n <= 0)
      Left("Classification is only possible for natural numbers.")
    else if (sumOfFactors < n)
      Right(NumberType.Deficient)
    else if (sumOfFactors > n)
      Right(NumberType.Abundant)
    else
      Right(NumberType.Perfect)
  }
}

sealed abstract class NumberType

object NumberType {
  case object Deficient extends NumberType
  case object Abundant extends NumberType
  case object Perfect extends NumberType
}
