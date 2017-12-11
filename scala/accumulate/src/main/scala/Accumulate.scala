import scala.annotation.tailrec

// Why is this a class?
// It's a stateless object, no need to instanciate it.
//
class Accumulate {
  def accumulate[A, B](f: (A) => B, list: List[A]): List[B] = {
    @tailrec
    def loop[A, B](f: (A) => B, acc: List[B], list: List[A]): List[B] =
      list match {
        case Nil     => acc
        case x :: xs => loop(f, f(x) :: acc, xs)
      }

    loop(f, List(), list).reverse
  }

}
