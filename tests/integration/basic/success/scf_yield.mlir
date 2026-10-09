// RUN: %reussir-opt %s | %reussir-opt

!opt_some = !reussir.record<compound "Opt::Some" [value] {i32}>
!opt_none = !reussir.record<compound "Opt::None" [value] {}>
!opt = !reussir.record<variant "Opt" {!opt_some, !opt_none}>

module {
  func.func @test_nullable_dispatch_with_yield(%nullable : !reussir.nullable<!reussir.ref<i32>>) -> i32 {
    %result = reussir.nullable.dispatch(%nullable : !reussir.nullable<!reussir.ref<i32>>) -> i32 {
      nonnull -> {
        ^bb0(%nonnull_ptr : !reussir.ref<i32>):
          %value = reussir.ref.load(%nonnull_ptr : !reussir.ref<i32>) : i32
          reussir.scf.yield %value : i32
      }
      null -> {
        ^bb0:
          %default = arith.constant 0 : i32
          reussir.scf.yield %default : i32
      }
    }
    func.return %result : i32
  }

  func.func @test_nullable_dispatch_void(%nullable : !reussir.nullable<!reussir.ref<i32>>) {
    reussir.nullable.dispatch(%nullable : !reussir.nullable<!reussir.ref<i32>>) {
      nonnull -> {
        ^bb0(%nonnull_ptr : !reussir.ref<i32>):
          reussir.scf.yield
      }
      null -> {
        ^bb0:
          reussir.scf.yield
      }
    }
    func.return
  }

  // A yield belongs to its immediate parent, not to the nearest dispatch
  // around it: a dispatch without results nested in an arm of one that
  // yields a value (a variant drop expanded inside a match arm), and a
  // dispatch yielding another type.
  func.func @test_nested_dispatch_yields(%nullable : !reussir.nullable<!reussir.ref<i32>>, %opt_ref : !reussir.ref<!opt>) -> i32 {
    %result = reussir.nullable.dispatch(%nullable : !reussir.nullable<!reussir.ref<i32>>) -> i32 {
      nonnull -> {
        ^bb0(%nonnull_ptr : !reussir.ref<i32>):
          reussir.record.dispatch(%opt_ref : !reussir.ref<!opt>) {
            [0] -> {
              ^bb0(%arg : !reussir.ref<!opt_some>):
                reussir.scf.yield
            }
            [1] -> {
              ^bb0(%arg : !reussir.ref<!opt_none>):
                reussir.scf.yield
            }
          }
          %value = reussir.ref.load(%nonnull_ptr : !reussir.ref<i32>) : i32
          reussir.scf.yield %value : i32
      }
      null -> {
        ^bb0:
          %wide = reussir.record.dispatch(%opt_ref : !reussir.ref<!opt>) -> i64 {
            [0, 1] -> {
              %c7 = arith.constant 7 : i64
              reussir.scf.yield %c7 : i64
            }
          }
          %narrow = arith.trunci %wide : i64 to i32
          reussir.scf.yield %narrow : i32
      }
    }
    func.return %result : i32
  }
}
