// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  reussir.func @test_str_literal_not_found() -> !reussir.str<global> {
    // expected-error @+1 {{referenced symbol is not a reussir.str.global: @nonexistent}}
    %str = reussir.str.literal @nonexistent : !reussir.str<global>
    return %str : !reussir.str<global>
  }
}

