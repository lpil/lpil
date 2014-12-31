use std::io;
use std::rand;

fn main() {
    println!("Guess the number");

    // Generate a random unsigned int between 1 - 100 inclusive
    let secret_number = (rand::random::<uint>() % 100) + 1;
    println!("The secret number is {}", secret_number);

    // Try to read an unsigned int. See option type for error handling
    println!("Please input your guess");
    let input: Option<uint> = 
        io::stdin().read_line()
                   .ok() // Convert to Option type from ioResult type
                   .expect("Failed to read line") // Handle None
                   .trim() // Remove trailing newline
                   .parse(); // Convert to uint

    // Unwrap the uint from the option
    let num: uint = match input {
        Some(x) => x,
        None    => {
            println!("Please input a number!");
            return;
        }
    };

    println!("You guessed {}", num);

    println!("{}", match cmp(num, secret_number) {
        Less    => "Too small!",
        Greater => "Too big!",
        Equal   => "You win!"
    });
}

fn cmp(a: uint, b: uint) -> Ordering {
    if a < b { Less }
    else if a > b { Greater }
    else { Equal }
}
