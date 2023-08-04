#![feature(c_variadic)]

use core::fmt;
use std::ffi::{c_char, c_double, c_int, CStr, VaList};

enum PlaceHolder {
    Int(c_int),
    Float(c_double),
    String(*const c_char),
    Boolean(c_int),
}

impl PlaceHolder {
    unsafe fn from_string(placeholder: &str, args: &mut VaList) -> PlaceHolder {
        match placeholder {
            "{integer}" => PlaceHolder::Int(args.arg::<c_int>()),
            "{boolean}" => PlaceHolder::Boolean(args.arg::<c_int>()),
            "{float}" => PlaceHolder::Float(args.arg::<c_double>()),
            "{string}" => PlaceHolder::String(args.arg::<*const c_char>()),
            &_ => {
                panic!("Invalid placeholder type")
            }
        }
    }
}

impl fmt::Display for PlaceHolder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlaceHolder::Int(x) => {
                write!(f, "{:?}", x)
            }
            PlaceHolder::Float(x) => {
                write!(f, "{:?}", x)
            }
            PlaceHolder::Boolean(x) => {
                let x = if *x == 0 { "false" } else { "true" };
                write!(f, "{}", x)
            }
            PlaceHolder::String(x) => unsafe {
                let cstring = CStr::from_ptr(*x);
                let outstr = cstring.to_str().unwrap();
                write!(f, "{}", outstr)
            },
        }
    }
}

unsafe fn format_str(input_str: &str, vars: &mut VaList) -> String {
    let mut result = String::new();
    let mut last_end = 0;

    let re = regex::Regex::new(r"\{[a-zA-Z]+\}").unwrap();

    for capture in re.find_iter(input_str) {
        let placeholder = PlaceHolder::from_string(capture.as_str(), vars);

        result.push_str(&input_str[last_end..capture.start()]);
        result.push_str(&placeholder.to_string());

        last_end = capture.end();
    }

    result.push_str(&input_str[last_end..]);
    result
}

/// Take a formatting string and a list of arguments and return a string
/// with the arguments inserted at the formatting characters.
///
/// # Example usage:
/// let s = fstring("Hey {} {} to {}!", 1, 2, 3);
/// println!("{}", s); // prints "Hey 1 2 to 3!"
#[no_mangle]
unsafe extern "C" fn print(str: *const c_char, mut vars: ...) {
    let input_str = CStr::from_ptr(str).to_str().unwrap();
    let result = format_str(input_str, &mut vars.as_va_list());
    println!("{}", result);
}

#[cfg(test)]
mod tests {
    use std::ffi::CString;

    use super::*;

    #[test]
    fn testprint() {
        unsafe {
            let format_str = CString::new("Hey {integer} {float}!").unwrap();
            print(format_str.as_ptr(), 1, 3.2 as c_double);

            let format_str = CString::new("Hey {string}!").unwrap();
            let inner_str = CString::new("formatting").unwrap();
            print(format_str.as_ptr(), inner_str.as_ptr());
        }
    }
}
