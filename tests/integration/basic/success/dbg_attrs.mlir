// RUN: mlir-opt %s --verify-roundtrip
module @"test" attributes {
    "reussir.test1" = #reussir.dbg_inttype< signed : true, i32, name : "i32" >,
    "reussir.test2" = #reussir.dbg_inttype< signed : false, i32, name : "u32" >,
    "reussir.test3" = #reussir.dbg_fptype<f64, name : "f64">
} {

}
