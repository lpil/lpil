class School {
  type DB = Map[Int, Seq[String]]

  // I dislike how this exercise forces mutable state
  var db: DB =
    Map()

  // Seems like Seq is implemented as a List.
  // Shouldn't we be prepending then as that would be O(1)?
  def add(name: String, g: Int) =
    db = db.updated(g, grade(g) :+ name)

  def grade(g: Int): Seq[String] =
    db.getOrElse(g, Seq())

  def sorted: DB = {
    val grades = db.toSeq.sortBy(_._1).map {
      case (k, v) =>
        (k, v.sorted)
    }
    Map(grades: _*)
  }
}
