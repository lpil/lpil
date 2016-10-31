use std::iter;
use std::str;
use super::Op;
use super::Sexpr;


pub fn parse_op(chars: &mut iter::Peekable<str::Chars>) -> Result<Op, String> {
    match chars.next() {
        None => Err("Unexpected EOF".to_string()),
        Some('+') => Ok(Op::Plus),
        Some('-') => Ok(Op::Minus),
        Some('/') => Ok(Op::Div),
        Some('*') => Ok(Op::Mult),
        Some(c) => Err(format!("Unexpected `{}`", c)),
    }
}


pub fn parse_num(chars: &mut iter::Peekable<str::Chars>) -> Result<Sexpr, String> {
    let mut point = false;
    let mut nums = String::new();
    while let Some(&c) = chars.peek() {
        if !point && c == '.' {
            point = true;
            nums.push(c)
        } else if c.is_digit(10) {
            nums.push(c)
        } else {
            break;
        }
        chars.next();
    }
    match nums.parse() {
        Ok(n) => Ok(Sexpr::Value(n)),
        Err(_) => Err("Invalid number".to_string()),
    }
}


mod tests {
    use super::*;
    use super::super::*;

    // parse_num

    #[test]
    fn parse_num_empty() {
        let mut chars = "".chars().peekable();
        let res = parse_num(&mut chars);
        assert_eq!(res, Err("Invalid number".to_string()));
    }

    #[test]
    fn parse_num_invalid() {
        let mut chars = "o".chars().peekable();
        let res = parse_num(&mut chars);
        assert_eq!(res, Err("Invalid number".to_string()));
    }

    #[test]
    fn parse_num_digit_then_letter() {
        let mut chars = "11o".chars().peekable();
        let res = parse_num(&mut chars);
        assert_eq!(res, Ok(Sexpr::Value(11.0)));
        assert_eq!(chars.peek(), Some(&'o'));
    }

    #[test]
    fn parse_num_float() {
        let mut chars = "23.45".chars().peekable();
        let res = parse_num(&mut chars);
        assert_eq!(res, Ok(Sexpr::Value(23.45)));
        assert_eq!(chars.peek(), None);
    }

    #[test]
    fn parse_num_float_then_dot() {
        let mut chars = "1.1.1".chars().peekable();
        let res = parse_num(&mut chars);
        assert_eq!(res, Ok(Sexpr::Value(1.1)));
        assert_eq!(chars.peek(), Some(&'.'));
    }

    #[test]
    fn parse_num_1_digit() {
        let mut chars = "5".chars().peekable();
        let res = parse_num(&mut chars);
        assert_eq!(res, Ok(Sexpr::Value(5.0)));
    }

    #[test]
    fn parse_num_2_digits() {
        let mut chars = "52".chars().peekable();
        let res = parse_num(&mut chars);
        assert_eq!(res, Ok(Sexpr::Value(52.0)));
    }

    #[test]
    fn parse_num_3_digits() {
        let mut chars = "524   ".chars().peekable();
        let res = parse_num(&mut chars);
        assert_eq!(res, Ok(Sexpr::Value(524.0)));
    }

    // parse_op

    #[test]
    fn parse_op_empty() {
        let mut chars = "".chars().peekable();
        let res = parse_op(&mut chars);
        assert_eq!(res, Err("Unexpected EOF".to_string()));
    }

    #[test]
    fn parse_op_plus() {
        let mut chars = "+".chars().peekable();
        let res = parse_op(&mut chars);
        assert_eq!(res, Ok(Op::Plus));
        assert_eq!(chars.peek(), None);
    }

    #[test]
    fn parse_op_minus() {
        let mut chars = "-".chars().peekable();
        let res = parse_op(&mut chars);
        assert_eq!(res, Ok(Op::Minus));
        assert_eq!(chars.peek(), None);
    }

    #[test]
    fn parse_op_div() {
        let mut chars = "/".chars().peekable();
        let res = parse_op(&mut chars);
        assert_eq!(res, Ok(Op::Div));
        assert_eq!(chars.peek(), None);
    }

    #[test]
    fn parse_op_mult() {
        let mut chars = "*".chars().peekable();
        let res = parse_op(&mut chars);
        assert_eq!(res, Ok(Op::Mult));
        assert_eq!(chars.peek(), None);
    }

    #[test]
    fn parse_op_other() {
        let mut chars = "?".chars().peekable();
        let res = parse_op(&mut chars);
        assert_eq!(res, Err("Unexpected `?`".to_string()));
        assert_eq!(chars.peek(), None);
    }
}
