class Pangram {
    companion object {
        val alphabet = "abcdefghijklmnopqrstuvwxyz".split("")

        fun isPangram(s: String): Boolean {
            return s.split("")
                    .map { it.toLowerCase() }
                    .toSet()
                    .containsAll(alphabet)
        }
    }
}
