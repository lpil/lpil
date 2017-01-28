#![allow(dead_code)]
// TODO

use cssparser::{Parser, Token, NumericValue, PercentageValue, RuleListParser, AtRuleType,
                AtRuleParser, QualifiedRuleParser, ToCss};
use rustc_serialize::json::{self, Json, ToJson};


struct XcssParser;

macro_rules! JArray {
    ($($e: expr,)*) => { JArray![ $( $e ),* ] };
    ($($e: expr),*) => { Json::Array(vec!( $( $e.to_json() ),* )) }
}

impl AtRuleParser for XcssParser {
    type Prelude = Vec<Json>;
    type AtRule = Json;

    fn parse_prelude(&mut self,
                     name: &str,
                     input: &mut Parser)
                     -> Result<AtRuleType<Vec<Json>, Json>, ()> {
        Ok(AtRuleType::OptionalBlock(vec!["at-rule".to_json(),
                                          name.to_json(),
                                          Json::Array(component_values_to_json(input))]))
    }

    fn parse_block(&mut self, mut prelude: Vec<Json>, input: &mut Parser) -> Result<Json, ()> {
        prelude.push(Json::Array(component_values_to_json(input)));
        Ok(Json::Array(prelude))
    }

    fn rule_without_block(&mut self, mut prelude: Vec<Json>) -> Json {
        prelude.push(Json::Null);
        Json::Array(prelude)
    }
}


impl QualifiedRuleParser for XcssParser {
    type Prelude = Vec<Json>;
    type QualifiedRule = Json;

    fn parse_prelude(&mut self, input: &mut Parser) -> Result<Vec<Json>, ()> {
        Ok(component_values_to_json(input))
    }

    fn parse_block(&mut self, prelude: Vec<Json>, input: &mut Parser) -> Result<Json, ()> {
        Ok(JArray![
            "qualified rule",
            prelude,
            component_values_to_json(input),
        ])
    }
}



fn component_values_to_json(input: &mut Parser) -> Vec<Json> {
    let mut values = vec![];
    while let Ok(token) = input.next_including_whitespace() {
        values.push(one_component_value_to_json(token, input));
    }
    values
}

fn one_component_value_to_json(token: Token, input: &mut Parser) -> Json {
    fn numeric(value: NumericValue) -> Vec<json::Json> {
        vec![Token::Number(value).to_css_string().to_json(),
             match value.int_value {
                 Some(i) => i.to_json(),
                 None => value.value.to_json(),
             },
             match value.int_value {
                     Some(_) => "integer",
                     None => "number",
                 }
                 .to_json()]
    }

    fn nested(input: &mut Parser) -> Vec<Json> {
        input.parse_nested_block(|input| Ok(component_values_to_json(input))).unwrap()
    }

    match token {
        Token::Ident(value) => JArray!["ident", value],
        Token::AtKeyword(value) => JArray!["at-keyword", value],
        Token::Hash(value) => JArray!["hash", value, "unrestricted"],
        Token::IDHash(value) => JArray!["hash", value, "id"],
        Token::QuotedString(value) => JArray!["string", value],
        Token::UnquotedUrl(value) => JArray!["url", value],
        Token::Delim('\\') => "\\".to_json(),
        Token::Delim(value) => value.to_string().to_json(),

        Token::Number(value) => {
            Json::Array({
                let mut v = vec!["number".to_json()];
                v.extend(numeric(value));
                v
            })
        }
        Token::Percentage(PercentageValue { unit_value, int_value, has_sign }) => {
            Json::Array({
                let mut v = vec!["percentage".to_json()];
                v.extend(numeric(NumericValue {
                    value: unit_value * 100.,
                    int_value: int_value,
                    has_sign: has_sign,
                }));
                v
            })
        }
        Token::Dimension(value, unit) => {
            Json::Array({
                let mut v = vec!["dimension".to_json()];
                v.extend(numeric(value));
                v.push(unit.to_json());
                v
            })
        }

        Token::UnicodeRange(start, end) => JArray!["unicode-range", start, end],

        Token::WhiteSpace(_) => " ".to_json(),
        Token::Comment(_) => "/**/".to_json(),
        Token::Colon => ":".to_json(),
        Token::Semicolon => ";".to_json(),
        Token::Comma => ",".to_json(),
        Token::IncludeMatch => "~=".to_json(),
        Token::DashMatch => "|=".to_json(),
        Token::PrefixMatch => "^=".to_json(),
        Token::SuffixMatch => "$=".to_json(),
        Token::SubstringMatch => "*=".to_json(),
        Token::Column => "||".to_json(),
        Token::CDO => "<!--".to_json(),
        Token::CDC => "-->".to_json(),

        Token::Function(name) => {
            Json::Array({
                let mut v = vec!["function".to_json(), name.to_json()];
                v.extend(nested(input));
                v
            })
        }
        Token::ParenthesisBlock => {
            Json::Array({
                let mut v = vec!["()".to_json()];
                v.extend(nested(input));
                v
            })
        }
        Token::SquareBracketBlock => {
            Json::Array({
                let mut v = vec!["[]".to_json()];
                v.extend(nested(input));
                v
            })
        }
        Token::CurlyBracketBlock => {
            Json::Array({
                let mut v = vec!["{}".to_json()];
                v.extend(nested(input));
                v
            })
        }
        Token::BadUrl => JArray!["error", "bad-url"],
        Token::BadString => JArray!["error", "bad-string"],
        Token::CloseParenthesis => JArray!["error", ")"],
        Token::CloseSquareBracket => JArray!["error", "]"],
        Token::CloseCurlyBracket => JArray!["error", "}"],
    }
}

pub fn parse(source: &str) {
    let mut input = Parser::new(source);
    let parser = RuleListParser::new_for_nested_rule(&mut input, XcssParser);
    let result = parser.map(|result| result.unwrap_or(JArray!["error", "invalid"])).collect();
    let json = Json::Array(result);
    println!("result: {:?}", json);
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_test() {
        parse(r#"
@function {}

main {
    color: hotpink;
}
"#);
        // assert!(false);
        // TODO
    }
}
