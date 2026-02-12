// RUN: %reussir-opt %s \
// RUN:   --reussir-lowering-scf-ops \
// RUN:   --convert-scf-to-cf \
// RUN:   --reussir-lowering-basic-ops | \
// RUN:   %reussir-translate --reussir-to-llvmir | \
// RUN:   %opt -S -O3 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %S/str_select_e2e.c %t.o -o %t.exe -L%library_path \
// RUN:    %rpath_flag %extra_sys_libs
// RUN: %t.exe

module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<f80, dense<128> : vector<2xi64>>, #dlti.dl_entry<i128, dense<128> : vector<2xi64>>, #dlti.dl_entry<i32, dense<32> : vector<2xi64>>, #dlti.dl_entry<f128, dense<128> : vector<2xi64>>, #dlti.dl_entry<f64, dense<64> : vector<2xi64>>, #dlti.dl_entry<f16, dense<16> : vector<2xi64>>, #dlti.dl_entry<i1, dense<8> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr, dense<64> : vector<4xi64>>, #dlti.dl_entry<!llvm.ptr<270>, dense<32> : vector<4xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>, #dlti.dl_entry<i16, dense<16> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr<272>, dense<64> : vector<4xi64>>, #dlti.dl_entry<!llvm.ptr<271>, dense<32> : vector<4xi64>>, #dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<"dlti.stack_alignment", 128 : i64>, #dlti.dl_entry<"dlti.endianness", "little">>, llvm.data_layout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"} {
  // Function to test basic pattern matching: matches "foo" (index 0), "bar" (index 1), or none
  reussir.func @test_basic(%str: !reussir.str<local>) -> (index, i1) {
    %idx, %found = reussir.str.select (%str) ["foo", "bar"] : (!reussir.str<local>) -> (index, i1)
    return %idx, %found : index, i1
  }

  // Function to test pattern with common prefix: "abc" (0), "abd" (1), "xyz" (2)
  reussir.func @test_prefix(%str: !reussir.str<local>) -> (index, i1) {
    %idx, %found = reussir.str.select (%str) ["abc", "abd", "xyz"] : (!reussir.str<local>) -> (index, i1)
    return %idx, %found : index, i1
  }

  // Function to test long pattern
  reussir.func @test_long(%str: !reussir.str<local>) -> (index, i1) {
    %idx, %found = reussir.str.select (%str) ["extremely_long_unique_pattern_that_is_over_32_chars"] : (!reussir.str<local>) -> (index, i1)
    return %idx, %found : index, i1
  }

  // Function to test many patterns with various first bytes
  reussir.func @test_many(%str: !reussir.str<local>) -> (index, i1) {
    %idx, %found = reussir.str.select (%str) ["apple", "banana", "cherry", "date", "elderberry"] : (!reussir.str<local>) -> (index, i1)
    return %idx, %found : index, i1
  }
}
