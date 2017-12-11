object SpaceAge {
  private val mercuryRatio: Double = 0.2408467
  private val venusRatio: Double = 0.61519726
  private val marsRatio: Double = 1.8808158
  private val jupiterRatio: Double = 11.862615
  private val saturnRatio: Double = 29.447498
  private val uranusRatio: Double = 84.016846
  private val neptuneRatio: Double = 164.79132

  def onEarth(n: Double): Double =
    round(n / 31557600)

  def onMercury(n: Double): Double =
    onEarth(n / mercuryRatio)

  def onVenus(n: Double): Double =
    onEarth(n / venusRatio)

  def onMars(n: Double): Double =
    onEarth(n / marsRatio)

  def onJupiter(n: Double): Double =
    onEarth(n / jupiterRatio)

  def onSaturn(n: Double): Double =
    onEarth(n / saturnRatio)

  def onUranus(n: Double): Double =
    onEarth(n / uranusRatio)

  def onNeptune(n: Double): Double =
    onEarth(n / neptuneRatio)

  private def round(n: Double): Double =
    (100 * n).round / 100.0
}
