extern crate golfscript;
extern crate copperline;

fn main() {
    let mut it = golfscript::Interpreter::new();
    let mut rl = copperline::Copperline::new();

    loop {
        match rl.read_line_utf8(">> ") {
            Ok(line) => {
                match it.exec(&line) {
                    Ok(response) => {
                        println!("{:?}", response);
                        rl.add_history(line);
                    }

                    Err(err) => {
                        println!("{:?}", err);
                    }
                }
            }

            Err(_) => break
        }
    }
}
