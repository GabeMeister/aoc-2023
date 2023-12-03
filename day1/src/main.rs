use std::{error::Error, fs};

#[derive(Debug, Clone)]
struct WordIndex {
    word: String,
    idx: usize,
}

const WORDS: [(&str, u8); 18] = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
    ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
];

// yxone5

fn get_first_number(text: &str) -> u8 {
    let mut temp: String = String::from("");

    for c in text.chars() {
        temp.push(c); // y

        for (w, i) in WORDS {
            if temp.ends_with(w) {
                return i;
            }
        }
    }

    return 0;
}

// xyonexy
// yxenoyx
// yx

fn get_last_number(text: &str) -> u8 {
    let mut temp: String = String::from("");

    for c in text.chars().rev() {
        temp.push(c); // y

        for (w, i) in WORDS {
            if temp.chars().rev().collect::<String>().starts_with(w) {
                return i;
            }
        }
    }

    return 0;
}

fn main() -> Result<(), Box<dyn Error>> {
    let text = fs::read_to_string("./src/input.txt")?;
    let lines: Vec<&str> = text.split("\n").collect();
    let mut total = 0;

    for line in lines {
        let mut line_copy = line.clone();

        dbg!(line_copy);

        let mut temp: String = String::from("");

        let first = get_first_number(line_copy);
        dbg!(first);
        let second = get_last_number(line_copy);
        dbg!(second);

        let combined = format!("{first}{second}").parse::<i32>().unwrap();
        dbg!(combined);
        total += combined;
    }

    dbg!(total);

    Ok(())
}
