mod games;
mod game_mechanics;

#[cfg(not(test))]
/// Why can't I get docs to work?
fn main() {
    let flag = std::os::args().pop().expect("");

    match flag.as_slice() {
        "launchpad" => games::launchpad_game(),
        _ => games::terminal_game()
    }
}
