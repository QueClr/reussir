// RUN: %reussir-opt %s --reussir-rc-create-fusion | %FileCheck %s

// Copy avoidance for a struct built in a reused cell: field i's store may be
// skipped when its value is a load of the old record's field i, but only if
// the old record has the same type. A cell of another struct of the same
// size and alignment can hold field i elsewhere: PA.1 is at offset 8 of PA,
// PB.1 at offset 4 of PB (12 when packed).

!pa = !reussir.record<compound "PA" {i64, i32}>
!pb = !reussir.record<compound "PB" {i32, i32, i64}>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i32, dense<32> : vector<2xi64>>> } {
  func.func @other_type(%rc: !reussir.rc<!pa>) -> !reussir.rc<!pb> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!pa>) : !reussir.ref<!pa>
    %f1_ref = reussir.ref.project (%borrow : !reussir.ref<!pa>) [1] : !reussir.ref<i32>
    %f1 = reussir.ref.load (%f1_ref : !reussir.ref<i32>) : i32
    %seven = arith.constant 7 : i32
    %nine = arith.constant 9 : i64
    %p = reussir.record.compound(%seven, %f1, %nine : i32, i32, i64) : !pb
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!pa>) : !reussir.token<align: 8, size: 24>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 24>) : !reussir.token<align: 8, size: 24>
    %new = reussir.rc.create value(%p : !pb) token(%token : !reussir.token<align: 8, size: 24>) skip_rc : !reussir.rc<!pb>
    return %new : !reussir.rc<!pb>
  }

  func.func @same_type(%rc: !reussir.rc<!pa>) -> !reussir.rc<!pa> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!pa>) : !reussir.ref<!pa>
    %f1_ref = reussir.ref.project (%borrow : !reussir.ref<!pa>) [1] : !reussir.ref<i32>
    %f1 = reussir.ref.load (%f1_ref : !reussir.ref<i32>) : i32
    %nine = arith.constant 9 : i64
    %p = reussir.record.compound(%nine, %f1 : i64, i32) : !pa
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!pa>) : !reussir.token<align: 8, size: 24>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 24>) : !reussir.token<align: 8, size: 24>
    %new = reussir.rc.create value(%p : !pa) token(%token : !reussir.token<align: 8, size: 24>) skip_rc : !reussir.rc<!pa>
    return %new : !reussir.rc<!pa>
  }
}

// CHECK-LABEL: func.func @other_type
// CHECK: reussir.rc.create_compound
// CHECK-NOT: skipFields
// CHECK: return

// CHECK-LABEL: func.func @same_type
// CHECK: reussir.rc.create_compound
// CHECK-SAME: skipFields = array<i64: 1>
