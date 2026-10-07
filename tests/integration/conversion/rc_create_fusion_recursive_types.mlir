// RUN: %reussir-opt %s --reussir-rc-create-fusion | %FileCheck %s

// Copy avoidance compares the reused cell's payload with the new one
// structurally. Two distinct recursive records with the same shape (a user
// list and the standard one) refer back to themselves, so the comparison of
// their recursive members must be coinductive: a pair already under
// comparison is assumed equal instead of being unfolded again (unfolding
// never terminates).

!mylist_ = !reussir.record<variant "MyList" incomplete>
!mycons = !reussir.record<compound "MyList::Cons" [value] {!mylist_, i64}>
!mynil = !reussir.record<compound "MyList::Nil" [value] {}>
!mylist = !reussir.record<variant "MyList" {!mycons, !mynil}>
!list_ = !reussir.record<variant "List" incomplete>
!cons = !reussir.record<compound "List::Cons" [value] {!list_, i64}>
!nil = !reussir.record<compound "List::Nil" [value] {}>
!list = !reussir.record<variant "List" {!cons, !nil}>
!other_ = !reussir.record<variant "Other" incomplete>
!othercons = !reussir.record<compound "Other::Cons" [value] {!other_, i64}>
!othernil = !reussir.record<compound "Other::Nil" [value] {i32}>
!other = !reussir.record<variant "Other" {!othercons, !othernil}>

// A [value] record stored inline and a shared one stored as a pointer are
// not the same type for layout purposes even when their members match.
!c_r = !reussir.record<compound "C::R" [value] {i64, i64}>
!c_b = !reussir.record<compound "C::B" [value] {}>
!cv = !reussir.record<variant "C" [value] {!c_r, !c_b}>
!d_r = !reussir.record<compound "D::R" [value] {i64, i64}>
!d_b = !reussir.record<compound "D::B" [value] {}>
!ds = !reussir.record<variant "D" {!d_r, !d_b}>
!s1a = !reussir.record<compound "S1::A" [value] {!cv, i64}>
!s1z = !reussir.record<compound "S1::Z" [value] {}>
!s1 = !reussir.record<variant "S1" {!s1a, !s1z}>
!s2a = !reussir.record<compound "S2::A" [value] {!ds, i64, i64, i64}>
!s2z = !reussir.record<compound "S2::Z" [value] {}>
!s2 = !reussir.record<variant "S2" {!s2a, !s2z}>

module {
  // S1::A = (C inline, 24 bytes; u64) and S2::A = (D pointer; u64, u64,
  // u64): field 1 sits at offset 24 in one and 8 in the other.
  func.func @value_vs_shared(%rc: !reussir.rc<!s1>, %d: !reussir.rc<!ds>) -> !reussir.rc<!s2> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!s1>) : !reussir.ref<!s1>
    %a_ref = reussir.record.coerce [0] (%borrow : !reussir.ref<!s1>) : !reussir.ref<!s1a>
    %v_ref = reussir.ref.project (%a_ref : !reussir.ref<!s1a>) [1] : !reussir.ref<i64>
    %v = reussir.ref.load (%v_ref : !reussir.ref<i64>) : i64
    %seven = arith.constant 7 : i64
    %nine = arith.constant 9 : i64
    %c = reussir.record.compound(%d, %v, %seven, %nine : !reussir.rc<!ds>, i64, i64, i64) : !s2a
    %var = reussir.record.variant [0] (%c : !s2a) : !s2
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!s1>) : !reussir.token<align: 8, size: 40>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 40>) : !reussir.token<align: 8, size: 40>
    %new = reussir.rc.create value(%var : !s2) token(%token : !reussir.token<align: 8, size: 40>) skip_rc : !reussir.rc<!s2>
    return %new : !reussir.rc<!s2>
  }

  // The cell of a MyList cons is reused for a List cons: the head keeps its
  // place, so its store is skipped.
  func.func @mylist_to_list(%rc: !reussir.rc<!mylist>, %tail: !reussir.rc<!list>) -> !reussir.rc<!list> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!mylist>) : !reussir.ref<!mylist>
    %cons_ref = reussir.record.coerce [0] (%borrow : !reussir.ref<!mylist>) : !reussir.ref<!mycons>
    %head_ref = reussir.ref.project (%cons_ref : !reussir.ref<!mycons>) [1] : !reussir.ref<i64>
    %head = reussir.ref.load (%head_ref : !reussir.ref<i64>) : i64
    %c = reussir.record.compound(%tail, %head : !reussir.rc<!list>, i64) : !cons
    %v = reussir.record.variant [0] (%c : !cons) : !list
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!mylist>) : !reussir.token<align: 8, size: 24>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 24>) : !reussir.token<align: 8, size: 24>
    %new = reussir.rc.create value(%v : !list) token(%token : !reussir.token<align: 8, size: 24>) skip_rc : !reussir.rc<!list>
    return %new : !reussir.rc<!list>
  }

  // Same shape up to the recursion, but the Nil arms differ: the recursive
  // members are not structurally equal, so nothing is skipped.
  func.func @other_to_list(%rc: !reussir.rc<!other>, %tail: !reussir.rc<!list>) -> !reussir.rc<!list> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!other>) : !reussir.ref<!other>
    %cons_ref = reussir.record.coerce [0] (%borrow : !reussir.ref<!other>) : !reussir.ref<!othercons>
    %head_ref = reussir.ref.project (%cons_ref : !reussir.ref<!othercons>) [1] : !reussir.ref<i64>
    %head = reussir.ref.load (%head_ref : !reussir.ref<i64>) : i64
    %c = reussir.record.compound(%tail, %head : !reussir.rc<!list>, i64) : !cons
    %v = reussir.record.variant [0] (%c : !cons) : !list
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!other>) : !reussir.token<align: 8, size: 24>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 24>) : !reussir.token<align: 8, size: 24>
    %new = reussir.rc.create value(%v : !list) token(%token : !reussir.token<align: 8, size: 24>) skip_rc : !reussir.rc<!list>
    return %new : !reussir.rc<!list>
  }
}

// CHECK-LABEL: func.func @value_vs_shared
// CHECK: "reussir.rc.create_variant"
// CHECK-NOT: skipFields
// CHECK: return

// CHECK-LABEL: func.func @mylist_to_list
// CHECK: "reussir.rc.create_variant"
// CHECK-SAME: skipFields = array<i64: 1>

// CHECK-LABEL: func.func @other_to_list
// CHECK: "reussir.rc.create_variant"
// CHECK-NOT: skipFields
// CHECK: return
