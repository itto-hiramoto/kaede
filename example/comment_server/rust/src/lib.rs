use serde_json::json;

fn normalize_spaces(input: &str) -> String {
    input
        .chars()
        .filter_map(|ch| match ch {
            '\r' | '\n' | '\t' => Some(' '),
            ch if ch.is_control() => None,
            ch => Some(ch),
        })
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

pub fn normalize_author(author: &str) -> String {
    normalize_spaces(author)
}

pub fn normalize_message(message: &str) -> String {
    normalize_spaces(message)
}

pub fn comment_json(id: u64, author: &str, message: &str) -> String {
    json!({
        "id": id,
        "author": author,
        "message": message,
    })
    .to_string()
}
