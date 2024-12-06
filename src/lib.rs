#![allow(dead_code, unused_variables)]

use wasm_bindgen::prelude::*;
use winnow::Parser;

mod syntax;
mod interpreter;

#[wasm_bindgen]
pub async fn main_js() {
    console_log::init_with_level(log::Level::Debug).unwrap();
    console_error_panic_hook::set_once();
    tracing_wasm::set_as_global_default();
    log::info!("hello computer, starting up...");
    let input = "([{x y} [{z} (+ x z)]] 1 2 3)";
    let e = syntax::parse_expr.parse_next(
        &mut winnow::stream::Located::new(input),
    ).expect("parse error");
    log::info!("parse success: {:?}", e);
    let mut i = interpreter::Interpreter::new();
    let res = i.eval(e);
    match res {
        Ok(x) => {
            log::info!("result: {}", x.pretty());
        },
        Err(e) => {
            log::error!("err: {:?}", e);
        }
    }
}
