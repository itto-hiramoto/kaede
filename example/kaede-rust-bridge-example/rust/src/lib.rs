pub fn greetings() {
    let url = "https://www.rust-lang.org";
    let rc = reqwest::blocking::get(url).unwrap();
    let contents = rc.text().unwrap();
    println!("{}", contents);
}

include!(concat!(env!("OUT_DIR"), "/kaede_bindings.rs"));
