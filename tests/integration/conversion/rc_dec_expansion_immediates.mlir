// RUN: %reussir-opt %s --split-input-file --reussir-rc-decrement-expansion | %FileCheck %s

// Under the special-pointer-tag scheme a nullary constructor is an immediate
// pointing at a static dummy box. Under TBI its increments land on the
// dummy's 32-bit count, which wraps after 2^32 references and can then read
// 1. The decrement therefore takes its unique branch only for a count of 1
// and a value that is not one of the type's immediates; an immediate takes
// the shared branch and releases nothing (no drop, no token).

!list_ = !reussir.record<variant "List" incomplete>
!list_nil = !reussir.record<compound "List::Nil" [value] { }>
!list_cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !list_nil, !list_cons }>
!pair_a = !reussir.record<compound "Pair::A" [value] { i64 }>
!pair_b = !reussir.record<compound "Pair::B" [value] { i64, i64 }>
!pair = !reussir.record<variant "Pair" { !pair_a, !pair_b }>

module attributes { reussir.special_ptr_tag = "tbi" } {
  // CHECK-LABEL: func.func @dec_list
  // CHECK: %[[CNT:.+]] = reussir.rc.fetch
  // CHECK: %[[ONE:.+]] = arith.cmpi eq, %[[CNT]]
  // CHECK: %[[IMM:.+]] = reussir.rc.compare_immortal(%arg0 : {{.+}}) tag(0)
  // CHECK: %[[NOT:.+]] = arith.xori %[[IMM]], %true
  // CHECK: %[[BOTH:.+]] = arith.andi %[[ONE]], %[[NOT]]
  // CHECK: %[[EXP:.+]] = reussir.expect(%[[BOTH]] : i1, true)
  // CHECK: scf.if %[[EXP]]
  // CHECK-NOT: compare_immortal
  // CHECK: reussir.ref.drop
  // CHECK: reussir.rc.reinterpret
  // CHECK: } else {
  func.func @dec_list(%rc: !reussir.rc<!list>) -> !reussir.nullable<!reussir.token<align: 8, size: 24>> {
    %token = reussir.rc.dec (%rc : !reussir.rc<!list>) : !reussir.nullable<!reussir.token<align: 8, size: 24>>
    return %token : !reussir.nullable<!reussir.token<align: 8, size: 24>>
  }

  // No nullary arm, no immediates: no test.
  // CHECK-LABEL: func.func @dec_pair
  // CHECK-NOT: compare_immortal
  // CHECK: return
  func.func @dec_pair(%rc: !reussir.rc<!pair>) -> !reussir.nullable<!reussir.token<align: 8, size: 24>> {
    %token = reussir.rc.dec (%rc : !reussir.rc<!pair>) : !reussir.nullable<!reussir.token<align: 8, size: 24>>
    return %token : !reussir.nullable<!reussir.token<align: 8, size: 24>>
  }
}

// -----

// The immortal encoding steers rc.inc's store away from the dummy, so the
// dummy's count never reaches 1: no test.

!list_ = !reussir.record<variant "List" incomplete>
!list_nil = !reussir.record<compound "List::Nil" [value] { }>
!list_cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !list_nil, !list_cons }>

module attributes { reussir.special_ptr_tag = "immortal" } {
  // CHECK-LABEL: func.func @dec_list_immortal
  // CHECK-NOT: compare_immortal
  // CHECK: reussir.ref.drop
  // CHECK-NOT: compare_immortal
  // CHECK: return
  func.func @dec_list_immortal(%rc: !reussir.rc<!list>) -> !reussir.nullable<!reussir.token<align: 8, size: 24>> {
    %token = reussir.rc.dec (%rc : !reussir.rc<!list>) : !reussir.nullable<!reussir.token<align: 8, size: 24>>
    return %token : !reussir.nullable<!reussir.token<align: 8, size: 24>>
  }
}
