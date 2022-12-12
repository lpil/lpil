object Raindrops {
    private val sounds = listOf(
        Rule(div = 3, noise = "Pling"),
        Rule(div = 5, noise = "Plang"),
        Rule(div = 7, noise = "Plong")
    )

    fun convert(n: Int): String {
        val result =
            sounds.filter { it.matches(n) }
                  .map { it.noise }
                  .joinToString("")

        return if (result.isEmpty()) {
            n.toString()
        } else {
            result
        }
    }
}

private data class Rule(private val div: Int, val noise: String) {
    fun matches(n: Int): Boolean {
        return n % div == 0
    }
}

