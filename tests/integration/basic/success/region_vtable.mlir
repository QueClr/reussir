// RUN: %reussir-opt %s | %FileCheck %s
module @test {
  // CHECK: reussir.func private @drop_func
  reussir.func private @drop_func(%ptr : !reussir.ref<i32>) -> () {
    return
  }
  
  // CHECK: reussir.region.vtable @vtable1
  reussir.region.vtable @vtable1 {
    type(i32)
    drop(@drop_func)
  }
}
