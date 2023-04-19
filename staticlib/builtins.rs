#![feature(c_variadic)]
#[no_mangle]
unsafe extern "C" fn print(x: i64) {
    println!("{}", x);
}
