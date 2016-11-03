use std::iter;
use std::str;
use super::Op;
use super::Sexpr;


pub fn parse(input: &String) -> Result<Sexpr, String> {
    let mut chars = input.chars().peekable();
    parse_list(&mut chars)
}

pub fn parse_op(chars: &mut iter::Peekable<str::Chars>) -> Result<Op, String> {
    let op = match chars.peek() {
        Some(&'+') => Ok(Op::Plus),
        Some(&'-') => Ok(Op::Minus),
        Some(&'/') => Ok(Op::Div),
        Some(&'*') => Ok(Op::Mult),
        Some(c) => return Err(format!("Invalid operator. Unexpected `{}`", c)),
        None => return Err("Invalid operator. Unexpected EOF".to_string()),
    };
    chars.next();
    op
}


pub fn parse_num(chars: &mut iter::Peekable<str::Chars>) -> Result<Sexpr, String> {
    let mut point = false;
    let mut nums = String::new();
    while let Some(&c) = chars.peek() {
        if !point && c == '.' {
            point = true;
            nums.push(c);
            chars.next();
        } else if c.is_digit(10) {
            nums.push(c);
            chars.next();
        } else {
            break;
        }
    }
    match nums.parse() {
        Ok(n) => Ok(Sexpr::Value(n)),
        Err(_) => Err("Invalid number".to_string()),
    }
}


pub fn parse_list(mut chars: &mut iter::Peekable<str::Chars>) -> Result<Sexpr, String> {
    if chars.peek() != Some(&'(') {
        return Err("Invalid list. Expected `(`".to_string());
    }
    chars.next();
    chomp(&mut chars);
    let op = try!(parse_op(&mut chars));
    let nums = parse_elems(&mut chars);
    if chars.peek() == Some(&')') {
        chars.next();
        Ok(Sexpr::List(op, nums))
    } else {
        Err("Invalid list. Expected `(`, `)` or number".to_string())
    }
}

fn parse_elems(mut chars: &mut iter::Peekable<str::Chars>) -> Vec<Sexpr> {
    let mut elems = vec![];
    loop {
        chomp(&mut chars);
        if let Ok(num) = parse_num(&mut chars) {
            elems.push(num);
            continue;
        }
        if let Ok(list) = parse_list(&mut chars) {
            elems.push(list);
            continue;
        }
        break;
    }
    elems
}

/// Drop preceeding spaces
///
fn chomp(chars: &mut iter::Peekable<str::Chars>) {
    while let Some(&c) = chars.peek() {
        if c == ' ' {
            chars.next();
        } else {
            break;
        }

    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::super::Op;
    use super::super::Sexpr;

    // Parse

    #[test]
    fn parse_test() {
        let input = "(+ 1 2 3)".to_string();
        let res = parse(&input);
        let nums = vec![Sexpr::Value(1.0), Sexpr::Value(2.0), Sexpr::Value(3.0)];
        let sexpr = Sexpr::List(Op::Plus, nums);
        assert_eq!(res, Ok(sexpr));
    }

    // parse_list

    #[test]
    fn parse_list_empty() {
        let mut chars = "".chars().peekable();
        let res = parse_list(&mut chars);
        assert_eq!(res, Err("Invalid list. Expected `(`".to_string()));
    }

    #[test]
    fn parse_list_of_num() {
        let mut chars = "(123)".chars().peekable();
        let res = parse_list(&mut chars);
        assert_eq!(res, Err("Invalid operator. Unexpected `1`".to_string()));
    }

    #[test]
    fn parse_list_of_op() {
        let mut chars = "(+)".chars().peekable();
        let res = parse_list(&mut chars);
        let sexpr = Sexpr::List(Op::Plus, vec![]);
        assert_eq!(res, Ok(sexpr));
    }

    #[test]
    fn parse_list_of_op_and_num() {
        let mut chars = "(/ 123)".chars().peekable();
        let res = parse_list(&mut chars);
        let sexpr = Sexpr::List(Op::Div, vec![Sexpr::Value(123.0)]);
        assert_eq!(res, Ok(sexpr));
    }

    #[test]
    fn parse_list_of_op_and_num_and_op() {
        let mut chars = "(+ 123 +)".chars().peekable();
        let res = parse_list(&mut chars);
        assert_eq!(res,
                   Err("Invalid list. Expected `(`, `)` or number".to_string()));
    }

    #[test]
    fn parse_incomplete_list() {
        let mut chars = "(+ 123".chars().peekable();
        let res = parse_list(&mut chars);
        assert_eq!(res,
                   Err("Invalid list. Expected `(`, `)` or number".to_string()));
    }


    #[test]
    fn parse_multi_num_list() {
        let mut chars = "(+ 1 2 3)".chars().peekable();
        let res = parse_list(&mut chars);
        let nums = vec![Sexpr::Value(1.0), Sexpr::Value(2.0), Sexpr::Value(3.0)];
        let sexpr = Sexpr::List(Op::Plus, nums);
        assert_eq!(res, Ok(sexpr));
    }

    #[test]
    fn parse_nested_list() {
        let mut chars = "(+ 1 (- 3))".chars().peekable();
        let res = parse_list(&mut chars);
        let sexpr1 = Sexpr::List(Op::Minus, vec![Sexpr::Value(3.0)]);
        let sexpr2 = Sexpr::List(Op::Plus, vec![Sexpr::Value(1.0), sexpr1]);
        assert_eq!(res, Ok(sexpr2));
    }

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
        assert_eq!(res, Err("Invalid operator. Unexpected EOF".to_string()));
        assert_eq!(chars.peek(), None);
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
        assert_eq!(res, Err("Invalid operator. Unexpected `?`".to_string()));
        assert_eq!(chars.peek(), Some(&'?'));
    }
}
