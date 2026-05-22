mod driver_test_support;

use driver_test_support::run_program as test;

#[test]
fn std_cmp_byte_slice_compare_orders_lexicographically() -> anyhow::Result<()> {
    test(
        0,
        r#"import std.cmp

use std.cmp.Ordering
use std.cmp.Ord

fun same<T: Ord>(left: T, right: T) -> bool {
    return left.compare(right) == Ordering::Equal
}

fun main() -> i32 {
    if b"abc".compare(b"abc") != Ordering::Equal {
        return 1
    }
    if b"abc".compare(b"abd") != Ordering::Less {
        return 2
    }
    if b"abd".compare(b"abc") != Ordering::Greater {
        return 3
    }
    if b"ab".compare(b"abc") != Ordering::Less {
        return 4
    }
    if b"abc".compare(b"ab") != Ordering::Greater {
        return 5
    }
    if !same(b"kaede", b"kaede") {
        return 6
    }

    return 0
}"#,
    )
}
