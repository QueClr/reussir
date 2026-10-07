// RUN: %reussir-opt %s --reussir-rc-create-fusion | %FileCheck %s

// Copy avoidance for a variant built in a reused cell: the store of field i
// is skipped when its value is a load of field i of the old arm, and field i
// has the same type and the same byte offset from the start of the box in
// both arms. The other members of the two arms may differ. The packed record
// layout (the default) sorts an arm's members by alignment: M::A = (i32, i64)
// puts A.0 at offset 8 of its payload, M::B = (i32, i32, i32) puts B.0 at
// offset 0.

!a = !reussir.record<compound "M::A" [value] {i32, i64}>
!b = !reussir.record<compound "M::B" [value] {i32, i32, i32}>
!m = !reussir.record<variant "M" {!a, !b}>
!c = !reussir.record<compound "N::C" [value] {i64, i32}>
!d = !reussir.record<compound "N::D" [value] {i64, i32, i32}>
!n = !reussir.record<variant "N" {!c, !d}>
!some = !reussir.record<compound "O::Some" [value] {i8}>
!none = !reussir.record<compound "O::None" [value] {}>
!o = !reussir.record<variant "O" {!some, !none}>
!ok = !reussir.record<compound "R::Ok" [value] {i8}>
!err = !reussir.record<compound "R::Err" [value] {i64}>
!r = !reussir.record<variant "R" {!ok, !err}>
!pa = !reussir.record<compound "P::A" [value] {f32, i32}>
!pz = !reussir.record<compound "P::Z" [value] {}>
!p = !reussir.record<variant "P" {!pa, !pz}>
!qb = !reussir.record<compound "Q::B" [value] {i32, i32}>
!qz = !reussir.record<compound "Q::Z" [value] {}>
!q = !reussir.record<variant "Q" {!qb, !qz}>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i32, dense<32> : vector<2xi64>>> } {
  func.func @field_moves(%rc: !reussir.rc<!m>) -> !reussir.rc<!m> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!m>) : !reussir.ref<!m>
    %a_ref = reussir.record.coerce [0] (%borrow : !reussir.ref<!m>) : !reussir.ref<!a>
    %f0_ref = reussir.ref.project (%a_ref : !reussir.ref<!a>) [0] : !reussir.ref<i32>
    %f0 = reussir.ref.load (%f0_ref : !reussir.ref<i32>) : i32
    %one = arith.constant 1 : i32
    %zero = arith.constant 0 : i32
    %p = reussir.record.compound(%f0, %one, %zero : i32, i32, i32) : !b
    %v = reussir.record.variant [1] (%p : !b) : !m
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!m>) : !reussir.token<align: 8, size: 24>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 24>) : !reussir.token<align: 8, size: 24>
    %new = reussir.rc.create value(%v : !m) token(%token : !reussir.token<align: 8, size: 24>) skip_rc : !reussir.rc<!m>
    return %new : !reussir.rc<!m>
  }

  // N::C = (i64, i32) and N::D = (i64, i32, i32) agree on offsets 0 and 8.
  func.func @fields_stay(%rc: !reussir.rc<!n>) -> !reussir.rc<!n> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!n>) : !reussir.ref<!n>
    %c_ref = reussir.record.coerce [0] (%borrow : !reussir.ref<!n>) : !reussir.ref<!c>
    %f0_ref = reussir.ref.project (%c_ref : !reussir.ref<!c>) [0] : !reussir.ref<i64>
    %f0 = reussir.ref.load (%f0_ref : !reussir.ref<i64>) : i64
    %f1_ref = reussir.ref.project (%c_ref : !reussir.ref<!c>) [1] : !reussir.ref<i32>
    %f1 = reussir.ref.load (%f1_ref : !reussir.ref<i32>) : i32
    %seven = arith.constant 7 : i32
    %p = reussir.record.compound(%f0, %f1, %seven : i64, i32, i32) : !d
    %v = reussir.record.variant [1] (%p : !d) : !n
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!n>) : !reussir.token<align: 8, size: 24>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 24>) : !reussir.token<align: 8, size: 24>
    %new = reussir.rc.create value(%v : !n) token(%token : !reussir.token<align: 8, size: 24>) skip_rc : !reussir.rc<!n>
    return %new : !reussir.rc<!n>
  }

  // A cell of another variant: O's arms are byte-aligned and R's are 8-byte
  // aligned, but both payloads start right after the 8-byte fused header.
  func.func @other_variant_same_offset(%rc: !reussir.rc<!o>) -> !reussir.rc<!r> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!o>) : !reussir.ref<!o>
    %s_ref = reussir.record.coerce [0] (%borrow : !reussir.ref<!o>) : !reussir.ref<!some>
    %f0_ref = reussir.ref.project (%s_ref : !reussir.ref<!some>) [0] : !reussir.ref<i8>
    %f0 = reussir.ref.load (%f0_ref : !reussir.ref<i8>) : i8
    %p = reussir.record.compound(%f0 : i8) : !ok
    %v = reussir.record.variant [0] (%p : !ok) : !r
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!o>) : !reussir.token<align: 8, size: 16>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 16>) : !reussir.token<align: 8, size: 16>
    %new = reussir.rc.create value(%v : !r) token(%token : !reussir.token<align: 8, size: 16>) skip_rc : !reussir.rc<!r>
    return %new : !reussir.rc<!r>
  }

  // P::A = (f32, i32) and Q::B = (i32, i32) differ at member 0, but member 1
  // has the same type and the same offset in both.
  func.func @other_member_before(%rc: !reussir.rc<!p>) -> !reussir.rc<!q> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!p>) : !reussir.ref<!p>
    %a_ref = reussir.record.coerce [0] (%borrow : !reussir.ref<!p>) : !reussir.ref<!pa>
    %f1_ref = reussir.ref.project (%a_ref : !reussir.ref<!pa>) [1] : !reussir.ref<i32>
    %f1 = reussir.ref.load (%f1_ref : !reussir.ref<i32>) : i32
    %three = arith.constant 3 : i32
    %c = reussir.record.compound(%three, %f1 : i32, i32) : !qb
    %v = reussir.record.variant [0] (%c : !qb) : !q
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!p>) : !reussir.token<align: 8, size: 16>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 16>) : !reussir.token<align: 8, size: 16>
    %new = reussir.rc.create value(%v : !q) token(%token : !reussir.token<align: 8, size: 16>) skip_rc : !reussir.rc<!q>
    return %new : !reussir.rc<!q>
  }

  // The same arm keeps every member where it was.
  func.func @same_arm(%rc: !reussir.rc<!m>) -> !reussir.rc<!m> {
    %borrow = reussir.rc.borrow (%rc : !reussir.rc<!m>) : !reussir.ref<!m>
    %a_ref = reussir.record.coerce [0] (%borrow : !reussir.ref<!m>) : !reussir.ref<!a>
    %f0_ref = reussir.ref.project (%a_ref : !reussir.ref<!a>) [0] : !reussir.ref<i32>
    %f0 = reussir.ref.load (%f0_ref : !reussir.ref<i32>) : i32
    %nine = arith.constant 9 : i64
    %p = reussir.record.compound(%f0, %nine : i32, i64) : !a
    %v = reussir.record.variant [0] (%p : !a) : !m
    %token0 = reussir.rc.reinterpret (%rc : !reussir.rc<!m>) : !reussir.token<align: 8, size: 24>
    %token = reussir.token.launder (%token0 : !reussir.token<align: 8, size: 24>) : !reussir.token<align: 8, size: 24>
    %new = reussir.rc.create value(%v : !m) token(%token : !reussir.token<align: 8, size: 24>) skip_rc : !reussir.rc<!m>
    return %new : !reussir.rc<!m>
  }
}

// CHECK-LABEL: func.func @field_moves
// CHECK: "reussir.rc.create_variant"
// CHECK-NOT: skipFields
// CHECK: return

// CHECK-LABEL: func.func @fields_stay
// CHECK: "reussir.rc.create_variant"
// CHECK-SAME: skipFields = array<i64: 0, 1>

// CHECK-LABEL: func.func @other_variant_same_offset
// CHECK: "reussir.rc.create_variant"
// CHECK-SAME: skipFields = array<i64: 0>

// CHECK-LABEL: func.func @other_member_before
// CHECK: "reussir.rc.create_variant"
// CHECK-SAME: skipFields = array<i64: 1>

// CHECK-LABEL: func.func @same_arm
// CHECK: "reussir.rc.create_variant"
// CHECK-SAME: skipFields = array<i64: 0>
