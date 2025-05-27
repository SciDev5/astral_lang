use std::iter::repeat;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArbitraryInt {
    negate: bool,
    chunks: Vec<u64>,
}
impl ArbitraryInt {
    /// Parse the integer (assumes the string represents a valid integer).
    pub fn parse(str: &str) -> Self {
        fn add_raw(a: Vec<u64>, b: Vec<u64>) -> Vec<u64> {
            let mut overflow = 0;
            let len = a.len().max(b.len()) + 1;
            let mut c = a
                .into_iter()
                .chain(repeat(0))
                .zip(b.into_iter().chain(repeat(0)))
                .take(len)
                .map(|(a, b)| {
                    let (c, overflow_0) = a.overflowing_add(b);
                    let (d, overflow_1) = c.overflowing_add(overflow);
                    overflow = (if overflow_0 { 1 } else { 0 }) + (if overflow_1 { 1 } else { 0 });
                    d
                })
                .collect::<Vec<_>>();
            while c.last().is_some_and(|v| *v == 0) {
                c.pop();
            }
            c
        }
        fn lshift_raw(a: &mut Vec<u64>, n: u32) {
            a.push(0);
            for i in (0..a.len() - 1).rev() {
                a[i + 1] |= a[i] >> (64 - n);
                a[i] = a[i].wrapping_shl(n);
            }
            if *a.last().unwrap() == 0 {
                a.pop();
            }
        }
        fn str_digits(str: &str, mul_base: impl Fn(Vec<u64>) -> Vec<u64>) -> Vec<u64> {
            let mut value = Vec::new();
            for ch in str.chars() {
                let digit = match ch {
                    '_' => continue,
                    '0' => 0,
                    '1' => 1,
                    '2' => 2,
                    '3' => 3,
                    '4' => 4,
                    '5' => 5,
                    '6' => 6,
                    '7' => 7,
                    '8' => 8,
                    '9' => 9,
                    'a' | 'A' => 10,
                    'b' | 'B' => 11,
                    'c' | 'C' => 12,
                    'd' | 'D' => 13,
                    'e' | 'E' => 14,
                    'f' | 'F' => 15,
                    _ => unreachable!("digits were invalid"),
                };
                value = add_raw(mul_base(value), Vec::from([digit]));
            }
            value
        }
        #[inline]
        fn take_prefix<'a>(str: &'a str, prefix: &str) -> (&'a str, bool) {
            if str.starts_with(prefix) {
                (&str[prefix.len()..], true)
            } else {
                (str, false)
            }
        }

        let (str, negate) = take_prefix(str, "-");
        let (str2, base2) = take_prefix(str, "0b");
        let (str16, base16) = take_prefix(str, "0x");

        Self {
            negate,
            chunks: if base2 {
                str_digits(str2, |mut n| {
                    lshift_raw(&mut n, 1);
                    n
                })
            } else if base16 {
                str_digits(str16, |mut n| {
                    lshift_raw(&mut n, 4);
                    n
                })
            } else {
                str_digits(str, |n| {
                    let mut times2 = n.clone();
                    let mut times8 = n;
                    lshift_raw(&mut times2, 1);
                    lshift_raw(&mut times8, 3);
                    add_raw(times2, times8)
                })
            },
        }
    }
}

#[test]
fn arbitraryint_from_str() {
    assert_eq!(
        ArbitraryInt::parse("-1"),
        ArbitraryInt {
            negate: true,
            chunks: Vec::from([1]),
        }
    );
    assert_eq!(
        ArbitraryInt::parse("12"),
        ArbitraryInt {
            negate: false,
            chunks: Vec::from([12]),
        }
    );
    assert_eq!(
        ArbitraryInt::parse("18446744073709551617"),
        ArbitraryInt {
            negate: false,
            chunks: Vec::from([1, 1]),
        }
    );
    assert_eq!(
        ArbitraryInt::parse("3066624690691497432836672575025617955718354"),
        ArbitraryInt {
            negate: false,
            chunks: Vec::from([1234, 5678, 9012]),
        }
    );
    assert_eq!(
        ArbitraryInt::parse("-0x2048"),
        ArbitraryInt {
            negate: true,
            chunks: Vec::from([0x2048]),
        }
    );
    assert_eq!(
        ArbitraryInt::parse("0x_0000000000001234_0000000000005678_0000000000009aBC"),
        ArbitraryInt {
            negate: false,
            chunks: Vec::from([0x9abc, 0x5678, 0x1234]),
        }
    );
}
