// Take any positive integer n. If n is even, divide n by 2 to get n / 2. If n
// is odd, multiply n by 3 and add 1 to get 3n + 1. Repeat the process
// indefinitely. The conjecture states that no matter which number you start
// with, you will always reach 1 eventually.
//
// Given a number n, return the number of steps required to reach 1.
//
object CollatzConjecture {
  def steps(n: Int): Option[Int] =
    if (n == 1)
      Some(0)
    else if (n < 1)
      None
    else if (n % 2 == 0)
      steps(n / 2).map(_ + 1)
    else
      steps(n * 3 + 1).map(_ + 1)
}
