// RUN: %reussir-opt %s -verify-diagnostics

// Test that moduleTexture cannot be empty

module @test {
  reussir.func @polyffi_empty_texture() {
    // expected-error @+1 {{moduleTexture cannot be empty}}
    reussir.polyffi texture("")
    reussir.return
  }
}

