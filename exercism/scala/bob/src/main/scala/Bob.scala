object Bob {
  def response(statement: String): String =
    if (statement.trim == "")
      "Fine. Be that way!"
    else if (hasLetters(statement) && allUpper(statement))
      "Whoa, chill out!"
    else if (statement.trim.endsWith("?"))
      "Sure."
    else
      "Whatever."

  private def hasLetters(string: String): Boolean =
    string.exists(_.isLetter)

  private def allUpper(string: String): Boolean =
    string.toUpperCase() == string
}
