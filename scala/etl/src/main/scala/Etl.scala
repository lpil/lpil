object Etl {
  def transform(scores: Map[Int, Seq[String]]): Map[String, Int] =
    scores.flatMap(expand).toMap

  private def expand(pair: (Int, Seq[String])): Seq[(String, Int)] =
    pair match {
      case (score, names) =>
        names.map(_.toLowerCase -> score)
    }
}
