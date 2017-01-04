// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -std=c++11 -include-pch %t -fsyntax-only -verify %s -ast-print | FileCheck %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

void foo() {}

template <typename T, int C>
T tmain(T argc, T *argv) {
  T i, j, a[20];
#pragma omp target teams
  foo();
#pragma omp target teams if (target:argc > 0)
  foo();
#pragma omp target teams if (C)
  foo();
#pragma omp target teams map(i)
  foo();
#pragma omp target teams map(a[0:10], i)
  foo();
#pragma omp target teams map(to: i) map(from: j)
  foo();
#pragma omp target teams map(always,alloc: i)
  foo();
#pragma omp target teams nowait
  foo();
#pragma omp target teams depend(in : argc, argv[i:argc], a[:])
  foo();
#pragma omp target teams defaultmap(tofrom: scalar)
  foo();
  return 0;
}

// CHECK: template <typename T, int C> T tmain(T argc, T *argv) {
// CHECK-NEXT: T i, j, a[20]
// CHECK-NEXT: #pragma omp target teams
// CHECK-NEXT: foo();
// CHECK-NEXT: #pragma omp target teams if(target: argc > 0)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams if(C)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(tofrom: i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(tofrom: a[0:10],i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(to: i) map(from: j)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(always,alloc: i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams nowait
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams depend(in : argc,argv[i:argc],a[:])
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams defaultmap(tofrom: scalar)
// CHECK-NEXT: foo()
// CHECK: template<> int tmain<int, 5>(int argc, int *argv) {
// CHECK-NEXT: int i, j, a[20]
// CHECK-NEXT: #pragma omp target teams
// CHECK-NEXT: foo();
// CHECK-NEXT: #pragma omp target teams if(target: argc > 0)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams if(5)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(tofrom: i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(tofrom: a[0:10],i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(to: i) map(from: j)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(always,alloc: i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams nowait
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams depend(in : argc,argv[i:argc],a[:])
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams defaultmap(tofrom: scalar)
// CHECK-NEXT: foo()
// CHECK: template<> char tmain<char, 1>(char argc, char *argv)
// CHECK-NEXT: char i, j, a[20]
// CHECK-NEXT: #pragma omp target teams
// CHECK-NEXT: foo();
// CHECK-NEXT: #pragma omp target teams if(target: argc > 0)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams if(1)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(tofrom: i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(tofrom: a[0:10],i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(to: i) map(from: j)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams map(always,alloc: i)
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams nowait
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams depend(in : argc,argv[i:argc],a[:])
// CHECK-NEXT: foo()
// CHECK-NEXT: #pragma omp target teams defaultmap(tofrom: scalar)
// CHECK-NEXT: foo()

// CHECK-LABEL: int main(int argc, char **argv) {
int main (int argc, char **argv) {
  int i, j, a[20];
// CHECK-NEXT: int i, j, a[20]
#pragma omp target teams
// CHECK-NEXT: #pragma omp target teams
  foo();
// CHECK-NEXT: foo();
#pragma omp target teams if (argc > 0)
// CHECK-NEXT: #pragma omp target teams if(argc > 0)
  foo();
// CHECK-NEXT: foo();

#pragma omp target teams map(i) if(argc>0)
// CHECK-NEXT: #pragma omp target teams map(tofrom: i) if(argc > 0)
  foo();
// CHECK-NEXT: foo();

#pragma omp target teams map(i)
// CHECK-NEXT: #pragma omp target teams map(tofrom: i)
  foo();
// CHECK-NEXT: foo();

#pragma omp target teams map(a[0:10], i)
// CHECK-NEXT: #pragma omp target teams map(tofrom: a[0:10],i)
  foo();
// CHECK-NEXT: foo();

#pragma omp target teams map(to: i) map(from: j)
// CHECK-NEXT: #pragma omp target teams map(to: i) map(from: j)
  foo();
// CHECK-NEXT: foo();

#pragma omp target teams map(always,alloc: i)
// CHECK-NEXT: #pragma omp target teams map(always,alloc: i)
  foo();
// CHECK-NEXT: foo();

#pragma omp target teams nowait
// CHECK-NEXT: #pragma omp target teams nowait
  foo();
// CHECK-NEXT: foo();

#pragma omp target teams depend(in : argc, argv[i:argc], a[:])
// CHECK-NEXT: #pragma omp target teams depend(in : argc,argv[i:argc],a[:])
  foo();
// CHECK-NEXT: foo();

#pragma omp target teams defaultmap(tofrom: scalar)
// CHECK-NEXT: #pragma omp target teams defaultmap(tofrom: scalar)
  foo();
// CHECK-NEXT: foo();

  return tmain<int, 5>(argc, &argc) + tmain<char, 1>(argv[0][0], argv[0]);
}

#endif
