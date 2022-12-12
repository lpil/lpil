import java.time.LocalDate
import java.time.LocalDateTime

data class Gigasecond(private val inputDateTime: LocalDateTime) {

    constructor(inputDate: LocalDate):
        this(inputDate.atTime(0, 0))

    val date = inputDateTime.plusSeconds(1000000000)
}
