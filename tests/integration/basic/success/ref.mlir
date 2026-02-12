// Pass twice to make sure the output is stable
// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s
module @test {
  // CHECK: reussir.func private @foo() -> !reussir.ref<index>
  reussir.func private @foo() -> !reussir.ref<index unspecified normal>

  // CHECK: reussir.func private @bar() -> !reussir.ref<index rigid>
  reussir.func private @bar() -> !reussir.ref<index rigid normal>

  // CHECK: reussir.func private @baz() -> !reussir.ref<index shared atomic>
  reussir.func private @baz() -> !reussir.ref<index shared atomic>

  // CHECK: reussir.func private @qux() -> !reussir.ref<index shared atomic>
  reussir.func private @qux() -> !reussir.ref<index atomic shared>
}
