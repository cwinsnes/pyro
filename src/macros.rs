#[macro_export]
macro_rules! try_unwrap_ok {
    ($e: expr, $err: expr) => {
        match $e {
            Ok(x) => x,
            _ => return Err($err),
        }
    }
}