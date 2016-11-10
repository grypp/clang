//===----- CGOpenMPRuntimeAnalyser.h - Interface to OpenMP Runtime Analyser
//-----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This provides a analyzing unit for automatically offloaded functions and
// statements.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_CODEGEN_CGOPENMPRUNTIMEANALYSER_H
#define LLVM_CLANG_LIB_CODEGEN_CGOPENMPRUNTIMEANALYSER_H

#include "CGValue.h"
#include "CodeGenFunction.h"
#include "clang/AST/Type.h"
#include "clang/Basic/OpenMPKinds.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/ValueHandle.h"

namespace llvm {
class ArrayType;
class Constant;
class FunctionType;
class GlobalVariable;
class StructType;
class Type;
class Value;
} // namespace llvm

namespace clang {
class Expr;
class GlobalDecl;
class OMPDependClause;
class OMPExecutableDirective;
class OMPLoopDirective;
class VarDecl;
class OMPDeclareReductionDecl;
class IdentifierInfo;

namespace CodeGen {

class CGOpenMPRuntimeAnalyser {
protected:
  CodeGenModule &CGM;

public:
  CGOpenMPRuntimeAnalyser(CodeGenModule &CGM) : CGM(CGM) {}
  virtual ~CGOpenMPRuntimeAnalyser() {}

  /// \brief Analyze statically current directive's region.
  /// \param CS Captured statement of region
  /// \param EntryFnName Unique name of stand-alone target function.
  virtual void AnalyseDirective(const OMPExecutableDirective &D,
                                llvm::StringRef EntryFnName);

  /// \brief Analyze statically current statement.
  /// \param CS Captured statement of region
  /// \param EntryFnName Unique name of stand-alone target function.
  virtual void AnalyseStatement(const Stmt &S, llvm::StringRef EntryFnName);
};

} // namespace CodeGen
} // namespace clang

#endif
