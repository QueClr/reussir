// Pass twice to make sure the output is stable
// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s
!foo = !reussir.record<compound "foo" {i32, i32}>
!list_cons = !reussir.record<compound "list.cons" [value] {i32, !reussir.record<variant "list" incomplete>}>
!list_nil = !reussir.record<compound "list.nil" [value] {}>
!list = !reussir.record<variant "list" {!list_cons, !list_nil}>
!tree = !reussir.record<variant "tree" {!reussir.record<compound "tree.branch" [value] {!reussir.record<variant "tree">, i32, !reussir.record<variant "tree">}>, !reussir.record<compound "tree.leaf" [value] {}>}>
module @test {
  // CHECK: reussir.func private @foo() -> !reussir.record<compound "foo" {i32, i32}>
  reussir.func private @foo() -> !foo
  // CHECK: reussir.func private @list() -> !reussir.record<variant "list" {!reussir.record<compound "list.cons" [value] {i32, !reussir.record<variant "list">}>, !reussir.record<compound "list.nil" [value] {}>}>
  reussir.func private @list() -> !list
  // CHECK: reussir.func private @tree() -> !reussir.record<variant "tree" {!reussir.record<compound "tree.branch" [value] {!reussir.record<variant "tree">, i32, !reussir.record<variant "tree">}>, !reussir.record<compound "tree.leaf" [value] {}>}
  reussir.func private @tree() -> !tree
}
