// RUN: %reussir-opt %s --reussir-rc-dispatch-fusion --reussir-rc-decrement-expansion | %FileCheck %s

// A destructuring decrement knows the arm of its box. Under TBI, an arm with
// members is always a real box, so its decrement gets no immediate compare.
// A nullary arm is an immediate, so its unique branch also requires that the
// value is not that arm's immediate.

!nilty = !reussir.record<compound "list.nil" [value] {}>
!consty = !reussir.record<compound "list.cons" [value] {i64, !reussir.record<variant "list" incomplete>}>
!listty = !reussir.record<variant "list" {!consty, !nilty}>
!rclist = !reussir.rc<!listty>
!tk = !reussir.token<align: 8, size: 24>
// CHECK-LABEL: func.func @take
// CHECK: [0] -> {
// CHECK-NOT: is_immediate
// CHECK: [1] -> {
// CHECK: %[[CNT:.+]] = reussir.rc.fetch
// CHECK: %[[ONE:.+]] = arith.cmpi eq, %[[CNT]]
// CHECK: %[[IMM:.+]] = reussir.rc.is_immediate(%arg0 : {{.+}})
// CHECK: %[[NOT:.+]] = arith.xori %[[IMM]], %true
// CHECK: %[[BOTH:.+]] = arith.andi %[[ONE]], %[[NOT]]
// CHECK: reussir.expect(%[[BOTH]] : i1, true)
module @test attributes {reussir.special_ptr_tag = "tbi", dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr, dense<64> : vector<4xi64>>, #dlti.dl_entry<"dlti.endianness", "little">>, llvm.data_layout = "e-m:e-i64:64-n8:16:32:64-S128"} {
  func.func @take(%l: !rclist, %d: !rclist) -> !rclist {
    %ref = reussir.rc.borrow (%l : !rclist) : !reussir.ref<!listty>
    %r = reussir.record.dispatch (%ref : !reussir.ref<!listty>) -> !rclist {
      [0] -> {
        ^bb0(%cons: !reussir.ref<!consty>):
        %slot = reussir.ref.project (%cons : !reussir.ref<!consty>) [1] : !reussir.ref<!rclist>
        %tail = reussir.ref.load (%slot : !reussir.ref<!rclist>) : !rclist
        reussir.rc.inc (%tail : !rclist)
        %t = reussir.rc.dec (%l : !rclist) : !reussir.nullable<!tk>
        reussir.scf.yield %tail : !rclist
      }
      [1] -> {
        ^bb1(%nil: !reussir.ref<!nilty>):
        %t = reussir.rc.dec (%l : !rclist) : !reussir.nullable<!tk>
        reussir.scf.yield %d : !rclist
      }
    }
    return %r : !rclist
  }
}
