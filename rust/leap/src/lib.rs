pub fn is_leap_year(year: u16) -> bool {
    (year % 4 == 0) && (year % 100 != 0) || (year % 400 == 0)
}
