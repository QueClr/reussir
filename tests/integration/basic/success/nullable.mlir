// RUN: %reussir-opt %s | %FileCheck %s
module @test {
  // CHECK: reussir.func private @foo() -> !reussir.nullable<!reussir.token<align : 8, size : 64>
  reussir.func private @foo() 
    -> !reussir.nullable<!reussir.token<align: 8, size: 64>>

}
