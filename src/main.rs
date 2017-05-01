extern crate drebbel;
extern crate linefeed;

use linefeed::{Reader, ReadResult};

fn main() {
    println!("Hello, drebbel!");

    let mut reader = Reader::new("drebbel").unwrap();

    reader.set_prompt("drebbel> ");

    while let Ok(ReadResult::Input(input)) = reader.read_line() {
        println!("got input {:?}", input);

        let drebbel_expression = drebbel::parse_Expression(input.as_str());

        match drebbel_expression {
            Ok(expr) => {
                println!("Got expression {:?}", expr);

                let value = drebbel::evaluate(*expr);

                println!("Evaluates to {:?}", value)
            },
            Err(err) => println!("Syntax error: {:?}", err)
        }

    }

    println!("Goodbye.");
}
