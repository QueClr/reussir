// RUN: %reussir-opt %s -split-input-file -verify-diagnostics
// The rc and ref types end at `>`: text after the element type and its
// keywords is an error. The parser used to stop at the first token that is
// not a keyword and drop the rest, so `!reussir.rc<i64 rigid, atomic>` read
// as `!reussir.rc<i64 rigid>`, a different type.

module @test {
  // expected-error @+1 {{expected '>'}}
  func.func private @f() -> !reussir.rc<i64, bogus words here>
}

// -----

module @test {
  // expected-error @+1 {{expected '>'}}
  func.func private @f() -> !reussir.rc<i64 rigid, atomic>
}

// -----

module @test {
  // expected-error @+1 {{expected '>'}}
  func.func private @f() -> !reussir.ref<i64, field>
}
