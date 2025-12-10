pub fn greetings() {
    let url = "https://www.rust-lang.org";
    let rc = reqwest::blocking::get(url).unwrap();
    let contents = rc.text().unwrap();
    println!("{}", contents);
}

pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn is_even(a: i32) -> bool {
    !is_odd(a)
}

fn is_odd(a: i32) -> bool {
    a % 2 != 0
}

include!(concat!(env!("OUT_DIR"), "/kaede_bindings.rs"));
