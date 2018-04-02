#[macro_use]
extern crate serde_derive;
extern crate toml;

use std::collections::HashMap;

#[derive(Deserialize, Serialize)]
struct Track {
    name: String,
    artist: String,
    genre: String,
    tags: Vec<String>,
}

fn main() {
    let mut tracks = HashMap::new();

    tracks.insert(
        "Detest - My Way",
        Track {
            name: "My Way".to_string(),
            artist: "Detest".to_string(),
            genre: "Crossbreed".to_string(),
            tags: vec!["Limp Bizkit".to_string(), "Kickin".to_string()],
        },
    );

    tracks.insert(
        "Bonobo & Nicole Miglis - Surface",
        Track {
            name: "Surface".to_string(),
            artist: "Bonobo & Nicole Miglis".to_string(),
            genre: "Downtempo".to_string(),
            tags: vec!["Gentle".to_string(), "Warm".to_string()],
        },
    );

    let toml = toml::to_string(&tracks).unwrap();
    println!("{}", toml);
}
