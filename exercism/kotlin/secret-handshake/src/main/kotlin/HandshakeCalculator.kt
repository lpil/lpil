/**
 * 1 = wink
 * 10 = double blink
 * 100 = close your eyes
 * 1000 = jump
 *
 *
 * 10000 = Reverse the order of the operations in the secret handshake.
 */
object HandshakeCalculator {

    fun calculateHandshake(n: Int): List<Signal> {
        val signals = Signal.values().filter { bitIsSet(it.ordinal, n) }

        return if (n >= 16) {
            signals.asReversed()
        } else {
            signals
        }
    }

    private fun bitIsSet(position: Int, number: Int): Boolean {
        return ((number shr position) and 1) == 1
    }
}
