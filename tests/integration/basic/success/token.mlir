// RUN: %reussir-opt %s | %FileCheck %s
!foo = !reussir.token<align: 8, size: 64>
module @test {
  // CHECK: reussir.func private @foo() -> !reussir.token<align : 8, size : 64>
  reussir.func private @foo() -> !foo
}
