class Hamming {
    companion object {
        fun compute(xs: String, ys: String): Int {
            if (xs.length != ys.length) {
                val msg = "left and right strands must be of equal length."
                throw IllegalArgumentException(msg)
            }
            return xs.zip(ys).count { (x, y) -> x != y }
        }
    }
}
