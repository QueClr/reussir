// Pass twice to make sure the output is stable
// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s
module @test {
  // CHECK: reussir.func private @foo() -> !reussir.rc<index>
  reussir.func private @foo() -> !reussir.rc<index shared normal>

  // CHECK: reussir.func private @bar() -> !reussir.rc<index rigid>
  reussir.func private @bar() -> !reussir.rc<index rigid normal>

  // CHECK: reussir.func private @baz() -> !reussir.rc<index atomic>
  reussir.func private @baz() -> !reussir.rc<index shared atomic>

  // CHECK: reussir.func private @qux() -> !reussir.rc<index atomic>
  reussir.func private @qux() -> !reussir.rc<index atomic shared>
}
