// RUN: %reussir-opt %s | %FileCheck %s
module @test {
  // CHECK: reussir.func private @foo() -> !reussir.region
  reussir.func private @foo() -> !reussir.region
}
