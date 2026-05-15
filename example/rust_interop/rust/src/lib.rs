pub fn greetings(url: &str) -> String {
    let rc = reqwest::blocking::get(url).unwrap();
    rc.text().unwrap()
}

pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
