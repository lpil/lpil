object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int =
    (1 to limit - 1).filter(isMultiple(factors)).sum

  def isMultiple(factors: Set[Int])(num: Int): Boolean =
    factors.exists(num % _ == 0)
}
