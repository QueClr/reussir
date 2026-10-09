// RUN: %reussir-opt %s --reussir-unique-carrying-recursion-analysis | %FileCheck %s

// Regression tests for the soundness and completeness rules of the
// uniqueness-carrying analysis (see the pass source header):
//   - provenance is disqualified by sharing (an rc.inc alias or a second
//     consuming user): asserting uniqueness on such a value would be UB;
//   - results are traced through `reussir.record.dispatch` arms (match-based
//     recursion, the dominant frontend shape);
//   - loop-carrying scf ops stay Unknown (their results may be the init
//     operands on a zero-trip count);
//   - `skip_rc` creates are not fresh (their count is not initialized here);
//   - a contributor of unknown provenance (an opaque call's result, a value
//     loaded from a field) absorbs the join: a value that is unknown on one
//     path and fresh on another is not unique;
//   - the poison of an unreachable arm and a nullary constructor's tagged
//     immediate are never shared heap cells: they do not block a
//     specialization.

!nilty = !reussir.record<compound "list.nil" [value] {}>
!consty = !reussir.record<compound "list.cons" [value] {i64, !reussir.record<variant "list" incomplete>}>
!listty = !reussir.record<variant "list" {!consty, !nilty}>
!rclist = !reussir.rc<!listty>
!tk = !reussir.token<align: 8, size: 24>

module @test attributes {reussir.special_ptr_tag = "tbi", dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr, dense<64> : vector<4xi64>>, #dlti.dl_entry<"dlti.endianness", "little">>, llvm.data_layout = "e-m:e-i64:64-n8:16:32:64-S128"} {
  // Completeness: results flowing through dispatch arms (a fresh create in
  // one arm, a carried argument in the other) make the function carrying,
  // and a self call on a fresh, unshared argument specializes.
  // CHECK-LABEL: func.func private @through_dispatch(
  // CHECK: attributes {llvm.linkage = #llvm.linkage<internal>, reussir.carrying_uniqueness}
  // CHECK: func.call @through_dispatch.unique(
  func.func private @through_dispatch(%l: !rclist, %n: i64) -> !rclist attributes {llvm.linkage = #llvm.linkage<internal>} {
    %ref = reussir.rc.borrow (%l : !rclist) : !reussir.ref<!listty>
    %r = reussir.record.dispatch (%ref : !reussir.ref<!listty>) -> !rclist {
      [0] -> {
        ^bb0(%cons: !reussir.ref<!consty>):
        %tag = reussir.record.compound : !nilty
        %v = reussir.record.variant [1] (%tag : !nilty) : !listty
        %fresh = reussir.rc.create value(%v : !listty) : !rclist
        %rec = func.call @through_dispatch(%fresh, %n) : (!rclist, i64) -> !rclist
        reussir.scf.yield %rec : !rclist
      }
      [1] -> {
        ^bb1(%nil: !reussir.ref<!nilty>):
        reussir.scf.yield %l : !rclist
      }
    }
    return %r : !rclist
  }

  // Soundness: the created value is retained (`rc.inc`) before the self
  // call — its count is 2 at the call, so no uniqueness may be assumed and
  // no specialization is created.
  // CHECK-LABEL: func.func private @shared_by_inc(
  // CHECK-NOT: func.call @shared_by_inc.unique
  // CHECK: return
  func.func private @shared_by_inc(%l: !rclist, %n: i64) -> !rclist attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c0 = arith.constant 0 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %r = scf.if %cond -> (!rclist) {
      scf.yield %l : !rclist
    } else {
      %tag = reussir.record.compound : !nilty
      %v = reussir.record.variant [1] (%tag : !nilty) : !listty
      %fresh = reussir.rc.create value(%v : !listty) : !rclist
      reussir.rc.inc(%fresh : !rclist)
      %rec = func.call @shared_by_inc(%fresh, %c0) : (!rclist, i64) -> !rclist
      reussir.rc.dec(%fresh : !rclist) : !reussir.nullable<!tk>
      scf.yield %rec : !rclist
    }
    return %r : !rclist
  }

  // Soundness: the created value has a second consuming user (another call)
  // besides the self call — conservatively rejected.
  // CHECK-LABEL: func.func private @shared_by_second_consumer(
  // CHECK-NOT: func.call @shared_by_second_consumer.unique
  // CHECK: return
  func.func private @sink(%l: !rclist) -> i64 attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c0 = arith.constant 0 : i64
    reussir.rc.dec(%l : !rclist) : !reussir.nullable<!tk>
    return %c0 : i64
  }
  func.func private @shared_by_second_consumer(%l: !rclist, %n: i64) -> !rclist attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c0 = arith.constant 0 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %r = scf.if %cond -> (!rclist) {
      scf.yield %l : !rclist
    } else {
      %tag = reussir.record.compound : !nilty
      %v = reussir.record.variant [1] (%tag : !nilty) : !listty
      %fresh = reussir.rc.create value(%v : !listty) : !rclist
      %ignored = func.call @sink(%fresh) : (!rclist) -> i64
      %rec = func.call @shared_by_second_consumer(%fresh, %c0) : (!rclist, i64) -> !rclist
      scf.yield %rec : !rclist
    }
    return %r : !rclist
  }

  // Soundness: a result flowing through `scf.for` is Unknown — on a
  // zero-trip count the loop returns its init operand, a path a yield-only
  // join would miss. The function must not be marked carrying.
  // CHECK-LABEL: func.func private @through_for(
  // CHECK-NOT: reussir.carrying_uniqueness
  // CHECK: return
  func.func private @through_for(%l: !rclist, %n: index) -> !rclist attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %r = scf.for %i = %c0 to %n step %c1 iter_args(%acc = %l) -> (!rclist) {
      %tag = reussir.record.compound : !nilty
      %v = reussir.record.variant [1] (%tag : !nilty) : !listty
      %fresh = reussir.rc.create value(%v : !listty) : !rclist
      reussir.rc.dec(%acc : !rclist) : !reussir.nullable<!tk>
      scf.yield %fresh : !rclist
    }
    return %r : !rclist
  }
  // Soundness: the self call's argument is an opaque call's result (possibly
  // shared) on one path and fresh on the other; the join is Unknown, so no
  // uniqueness may be assumed and the function, which may return that
  // argument, does not carry uniqueness either.
  // CHECK-LABEL: func.func private @opaque_or_fresh_arg(
  // CHECK-NOT: reussir.carrying_uniqueness
  // CHECK-NOT: func.call @opaque_or_fresh_arg.unique
  // CHECK: return
  func.func private @opaque(i64) -> !rclist
  func.func private @opaque_or_fresh_arg(%l: !rclist, %n: i64) -> !rclist attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c0 = arith.constant 0 : i64
    %c1 = arith.constant 1 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %r = scf.if %cond -> (!rclist) {
      scf.yield %l : !rclist
    } else {
      reussir.rc.dec(%l : !rclist) : !reussir.nullable<!tk>
      %odd = arith.cmpi eq, %n, %c1 : i64
      %y = scf.if %odd -> (!rclist) {
        %s = func.call @opaque(%n) : (i64) -> !rclist
        scf.yield %s : !rclist
      } else {
        %tag = reussir.record.compound : !nilty
        %v = reussir.record.variant [1] (%tag : !nilty) : !listty
        %fresh = reussir.rc.create value(%v : !listty) : !rclist
        scf.yield %fresh : !rclist
      }
      %n1 = arith.subi %n, %c1 : i64
      %rec = func.call @opaque_or_fresh_arg(%y, %n1) : (!rclist, i64) -> !rclist
      scf.yield %rec : !rclist
    }
    return %r : !rclist
  }

  // Soundness: a result that is a field loaded out of a cell (shared with
  // that cell) on one path and fresh on the other is not carrying.
  // CHECK-LABEL: func.func private @field_or_fresh(
  // CHECK-NOT: reussir.carrying_uniqueness
  // CHECK: return
  func.func private @field_or_fresh(%c: !reussir.ref<!consty>, %n: i64) -> !rclist attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c0 = arith.constant 0 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %r = scf.if %cond -> (!rclist) {
      %p = reussir.ref.project (%c : !reussir.ref<!consty>) [1] : !reussir.ref<!rclist>
      %t = reussir.ref.load (%p : !reussir.ref<!rclist>) : !rclist
      reussir.rc.inc(%t : !rclist)
      scf.yield %t : !rclist
    } else {
      %tag = reussir.record.compound : !nilty
      %v = reussir.record.variant [1] (%tag : !nilty) : !listty
      %fresh = reussir.rc.create value(%v : !listty) : !rclist
      scf.yield %fresh : !rclist
    }
    return %r : !rclist
  }
  // Completeness: the self call's argument is fresh on one path and, on the
  // other, the poison of an unreachable arm (what follows a panic) or a
  // nullary constructor's tagged immediate. Neither is a heap cell another
  // reference can see, so the call still goes to the `.unique` clone.
  // CHECK-LABEL: func.func private @poison_or_fresh(
  // CHECK: func.call @poison_or_fresh.unique(
  func.func private @poison_or_fresh(%l: !rclist, %n: i64) -> !rclist attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c0 = arith.constant 0 : i64
    %c1 = arith.constant 1 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %r = scf.if %cond -> (!rclist) {
      scf.yield %l : !rclist
    } else {
      reussir.rc.dec(%l : !rclist) : !reussir.nullable<!tk>
      %odd = arith.cmpi eq, %n, %c1 : i64
      %y = scf.if %odd -> (!rclist) {
        %p = ub.poison : !rclist
        scf.yield %p : !rclist
      } else {
        %tag = reussir.record.compound : !nilty
        %v = reussir.record.variant [1] (%tag : !nilty) : !listty
        %fresh = reussir.rc.create value(%v : !listty) : !rclist
        scf.yield %fresh : !rclist
      }
      %n1 = arith.subi %n, %c1 : i64
      %rec = func.call @poison_or_fresh(%y, %n1) : (!rclist, i64) -> !rclist
      scf.yield %rec : !rclist
    }
    return %r : !rclist
  }

  // CHECK-LABEL: func.func private @tagged_or_fresh(
  // CHECK: func.call @tagged_or_fresh.unique(
  func.func private @tagged_or_fresh(%l: !rclist, %n: i64) -> !rclist attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c0 = arith.constant 0 : i64
    %c1 = arith.constant 1 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %r = scf.if %cond -> (!rclist) {
      scf.yield %l : !rclist
    } else {
      reussir.rc.dec(%l : !rclist) : !reussir.nullable<!tk>
      %odd = arith.cmpi eq, %n, %c1 : i64
      %y = scf.if %odd -> (!rclist) {
        %t = reussir.rc.tagged tag(1) : !rclist
        scf.yield %t : !rclist
      } else {
        %tag = reussir.record.compound : !nilty
        %v = reussir.record.variant [1] (%tag : !nilty) : !listty
        %fresh = reussir.rc.create value(%v : !listty) : !rclist
        scf.yield %fresh : !rclist
      }
      %n1 = arith.subi %n, %c1 : i64
      %rec = func.call @tagged_or_fresh(%y, %n1) : (!rclist, i64) -> !rclist
      scf.yield %rec : !rclist
    }
    return %r : !rclist
  }
}
