// RUN: %reussir-opt %s -verify-diagnostics

// Test that either moduleTexture or compiledModule must be specified

module @test {
  reussir.func @polyffi_neither_specified() {
    // expected-error @+1 {{either moduleTexture or compiledModule must be specified}}
    reussir.polyffi
    reussir.return
  }
}

