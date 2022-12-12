use std::collections::BTreeMap;

pub type OldFormat = BTreeMap<i32, Vec<String>>;
pub type NewFormat = BTreeMap<String, i32>;

pub fn transform(old: &OldFormat) -> NewFormat {
    let mut data: NewFormat = BTreeMap::new();
    for (value, keys) in old {
        let score = value.clone();
        for key in keys {
            data.insert(key.to_lowercase(), score);
        }
    }
    data
}
