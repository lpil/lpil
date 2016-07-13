pub fn hello(x: Option<&'static str>) -> String {
    match x {
        Some(name) =>
            format!("Hello, {}!", name),
        None =>
            String::from("Hello, World!"),
    }
}
