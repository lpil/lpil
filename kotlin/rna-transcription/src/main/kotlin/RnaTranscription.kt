fun transcribeToRna(dna: String): String {
    return dna.map { baseToRna(it) }.joinToString("")
}

private fun baseToRna(c: Char): Char {
  return when (c) {
    'G' ->
      'C'

    'A' ->
      'U'

    'T' ->
      'A'

    else ->
      'G'
  }
}
