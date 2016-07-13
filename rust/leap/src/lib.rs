// on every year that is evenly divisible by 4
//   except every year that is evenly divisible by 100
//     unless the year is also evenly divisible by 400

pub fn is_leap_year(year: u16) -> bool {
    is_divisible(year, 4)
        && (!is_divisible(year, 100) || is_divisible(year, 400))

}

fn is_divisible(num: u16, div: u16) -> bool {
    num % div == 0
}
