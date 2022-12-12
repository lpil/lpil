class Squares(val num: Int)  {

    fun squareOfSum(): Int {
        val sum = (1..num).sum()
        return square(sum)
    }

    fun sumOfSquares(): Int {
        return (1..num).map(::square).sum()
    }

    fun difference(): Int {
        return squareOfSum() - sumOfSquares()
    }
}

private fun square(n: Int): Int {
    return n * n
}
