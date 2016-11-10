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
// This provides a class for OpenMP runtime code generation.
//
//===----------------------------------------------------------------------===//
#include "CGOpenMPRuntimeAnalyser.h"
#include "CGCXXABI.h"
#include "CGCleanup.h"
#include "CGOpenMPRuntime.h"
#include "CodeGenFunction.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/Analysis/Analyses/Consumed.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>

using namespace clang;
using namespace CodeGen;

static bool isIdenticalStmt(const ASTContext &Ctx, const Stmt *Stmt1,
                            const Stmt *Stmt2, bool IgnoreSideEffects);
#define PRINTER_STRING                                         \
  " ------------------- Simple OpenMP Code Region Extracter "  \
  "------------------- \n"                                     \
  "  #Total Memory Access						"                               \
  "	%d	\n"                                                     \
  "  #Coalasced Memory Access						"                           \
  "%d	\n"                                                      \
  "  #Less Coalasced Memory Access					"                       \
  "	%d	\n"                                                     \
  "  %%Coalasced Memory Access						"                          \
  "%d%%	\n"                                                    \
  "  #Control Flow							"                                     \
  "	%d	\n"                                                     \
  "  #Expression							"                                       \
  "	%d	\n"                                                     \
  "  #Statement							"                                        \
  "	%d	\n"                                                     \
  "  #For Loop								"                                        \
  "%d	\n"                                                      \
  "  #Nested For Loop							"                                  \
  "%d	\n"                                                      \
  "  #Collapse Level							"                                  \
  "%d	\n"                                                      \
  " ------------------- Simple OpenMP Code Region Extracter  " \
  "------------------- \n"                                     \
  "\n"
namespace {
class OMPAnalyseVisitor : public RecursiveASTVisitor<OMPAnalyseVisitor> {
private:
  ASTContext &Context;

  int CCoalascedAccess = 0;
  int CLessCoalascedAccess = 0;
  int CMemoryAccess = 0;
  int CNestedLevel = 0;
  /// Collapse level that is specified by collapse clause. It is assigned to 1
  /// as default.
  int CCollapseLevel = 1;

  /// Current iteration incremented DeclRefExpr
  DeclRefExpr *CurrentForIncDeclRefExpr;

  /// The map of statements to count values. llvm::DenseMap<std::string, int>
  /// &CountMap; A flag that is set when the current count should be recorded on
  /// the next statement, such as at the exit of a loop. bool
  /// RecordNextStmtCount = false;
  llvm::StringMap<int> &CountMap;

public:
  OMPAnalyseVisitor(ASTContext &C, llvm::StringMap<int> &CountMap)
      : Context(C), CountMap(CountMap) {}

  ~OMPAnalyseVisitor(){};

  void PrintDetails() {
      printf(PRINTER_STRING, CMemoryAccess, CCoalascedAccess,
             CLessCoalascedAccess, ((CCoalascedAccess * 100) / CMemoryAccess),
             CountMap["IfStmt"], CountMap["Expr"], CountMap["Stmt"],
             CountMap["ForStmt"], CNestedLevel, CCollapseLevel);
  }

  void RecordStmtCount(std::string T, const Stmt *S) {
    // if (RecordNextStmtCount) {
    CountMap[T]++;
    // RecordNextStmtCount = false;
    //}
  }

  bool VisitOMPExecutableDirective(const OMPExecutableDirective *D) {
    if (isa<OMPTargetDirective>(D))
      RecordStmtCount("OMPTargetDirective", D);
    else if (isa<OMPTargetTeamsDirective>(D))
      RecordStmtCount("OMPTargetTeamsDirective", D);
    else if (isa<OMPTargetTeamsDistributeDirective>(D))
      RecordStmtCount("OMPTargetTeamsDistributeDirective", D);
    else if (isa<OMPTargetTeamsDistributeParallelForDirective>(D))
      RecordStmtCount("OMPTargetTeamsDistributeParallelForDirective", D);
    else if (isa<OMPTargetTeamsDistributeParallelForSimdDirective>(D))
      RecordStmtCount("OMPTargetTeamsDistributeParallelForSimdDirective", D);
    else if (isa<OMPTargetTeamsDistributeSimdDirective>(D))
      RecordStmtCount("OMPTargetTeamsDistributeSimdDirective", D);
    else if (isa<OMPTargetParallelDirective>(D))
      RecordStmtCount("OMPTargetParallelDirective", D);
    else if (isa<OMPTargetParallelForDirective>(D))
      RecordStmtCount("OMPTargetParallelForDirective", D);
    else if (isa<OMPTargetParallelForSimdDirective>(D))
      RecordStmtCount("OMPTargetParallelForSimdDirective", D);
    else if (isa<OMPTargetSimdDirective>(D))
      RecordStmtCount("OMPTargetSimdDirective", D);

    for (auto &C : D->clauses()) {
      if (isa<OMPCollapseClause>(C)) {
        OMPCollapseClause *CC = cast<OMPCollapseClause>(C);
        if (IntegerLiteral *IL =
                dyn_cast<IntegerLiteral>(CC->getNumForLoops())) {
          CCollapseLevel = IL->getValue().getZExtValue();
        }
      }
    }

    return true;
  }

  bool VisitOMPBarrierDirective(const OMPBarrierDirective *D) {
    RecordStmtCount("OMPBarrierDirective", D);
    return true;
  }

  bool VisitStmt(const Stmt *S) {
    RecordStmtCount("Stmt", S);
    return true;
  }

  bool VisitIfStmt(const IfStmt *IS) {
    RecordStmtCount("IfStmt", IS);
    return true;
  }

  bool VisitExpr(const Expr *E) {
    RecordStmtCount("Expr", E);
    return true;
  }

  bool VisitArraySubscriptExpr(const ArraySubscriptExpr *E) {
    if (const ImplicitCastExpr *ICE =
            dyn_cast<ImplicitCastExpr>(E->getBase())) {
      if (isa<DeclRefExpr>(ICE->getSubExpr())) {
        CMemoryAccess++;
      }
    }

    if (const ImplicitCastExpr *ICE = dyn_cast<ImplicitCastExpr>(E->getIdx())) {
      if (const DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(ICE->getSubExpr())) {
        if (isIdenticalStmt(Context, CurrentForIncDeclRefExpr, DRE, false)) {
          QualType QT = E->getBase()->getType();
          if (QT->hasPointerRepresentation()) {
            if (!QT->getPointeeOrArrayElementType()
                     ->hasPointerRepresentation()) {
              CCoalascedAccess++;
            } else {
              CLessCoalascedAccess++;
            }
          }
        }
      }
    }
    return true;
  }

  bool VisitForStmt(const ForStmt *S) {
    RecordStmtCount("ForStmt", S);

    // We by pass inner for statements as they are not distributed among
    // threads. When collapse clause is active, we pay attention inner for
    // statements.
    if (CountMap["ForStmt"] != CCollapseLevel)
      return true;

    // OpenMP already forces for Canonical loop form. So analyzer will take a
    // look considering that.
    // OpenMP [2.6] Canonical loop form. Test-expr may be one of the following:
    //   ++var
    //   var++
    //   --var
    //   var--
    //   var += incr
    //   var -= incr
    //   var = var + incr
    //   var = incr + var
    //   var = var - incr
    if (const BinaryOperator *BO = dyn_cast<BinaryOperator>(S->getInc())) {
      if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(BO->getLHS()))
        CurrentForIncDeclRefExpr = DRE;
    }

    if (const UnaryOperator *UO = dyn_cast<UnaryOperator>(S->getInc())) {
      if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(UO->getSubExpr())) {
        CurrentForIncDeclRefExpr = DRE;
      }
    }

    return true;
  }
};
}

void CGOpenMPRuntimeAnalyser::AnalyseDirective(const OMPExecutableDirective &D,
                                               llvm::StringRef EntryFnName) {
  const CapturedStmt *CS = cast<CapturedStmt>(D.getAssociatedStmt());
  ASTContext &C = CGM.getContext();
  llvm::StringMap<int> CountMap;
  OMPAnalyseVisitor OMPVisit(C, CountMap);
  OMPVisit.TraverseStmt(const_cast<Stmt *>(CS->getCapturedStmt()));
  if (C.getLangOpts().OpenMPAutomatic)
	  OMPVisit.PrintDetails();
}

void CGOpenMPRuntimeAnalyser::AnalyseStatement(const Stmt &S,
                                               llvm::StringRef EntryFnName) {
  return;
}

static bool isIdenticalStmt(const ASTContext &Ctx, const Stmt *Stmt1,
                            const Stmt *Stmt2, bool IgnoreSideEffects) {
  if (!Stmt1 || !Stmt2) {
    return !Stmt1 && !Stmt2;
  }

  // If Stmt1 & Stmt2 are of different class then they are not
  // identical statements.
  if (Stmt1->getStmtClass() != Stmt2->getStmtClass())
    return false;

  const Expr *Expr1 = dyn_cast<Expr>(Stmt1);
  const Expr *Expr2 = dyn_cast<Expr>(Stmt2);

  if (Expr1 && Expr2) {
    // If Stmt1 has side effects then don't warn even if expressions
    // are identical.
    if (!IgnoreSideEffects && Expr1->HasSideEffects(Ctx))
      return false;
    // If either expression comes from a macro then don't warn even if
    // the expressions are identical.
    if ((Expr1->getExprLoc().isMacroID()) || (Expr2->getExprLoc().isMacroID()))
      return false;

    // If all children of two expressions are identical, return true.
    Expr::const_child_iterator I1 = Expr1->child_begin();
    Expr::const_child_iterator I2 = Expr2->child_begin();
    while (I1 != Expr1->child_end() && I2 != Expr2->child_end()) {
      if (!*I1 || !*I2 || !isIdenticalStmt(Ctx, *I1, *I2, IgnoreSideEffects))
        return false;
      ++I1;
      ++I2;
    }
    // If there are different number of children in the statements, return
    // false.
    if (I1 != Expr1->child_end())
      return false;
    if (I2 != Expr2->child_end())
      return false;
  }

  switch (Stmt1->getStmtClass()) {
  default:
    return false;
  case Stmt::CallExprClass:
  case Stmt::ArraySubscriptExprClass:
  case Stmt::OMPArraySectionExprClass:
  case Stmt::ImplicitCastExprClass:
  case Stmt::ParenExprClass:
  case Stmt::BreakStmtClass:
  case Stmt::ContinueStmtClass:
  case Stmt::NullStmtClass:
    return true;
  case Stmt::CStyleCastExprClass: {
    const CStyleCastExpr *CastExpr1 = cast<CStyleCastExpr>(Stmt1);
    const CStyleCastExpr *CastExpr2 = cast<CStyleCastExpr>(Stmt2);

    return CastExpr1->getTypeAsWritten() == CastExpr2->getTypeAsWritten();
  }
  case Stmt::ReturnStmtClass: {
    const ReturnStmt *ReturnStmt1 = cast<ReturnStmt>(Stmt1);
    const ReturnStmt *ReturnStmt2 = cast<ReturnStmt>(Stmt2);

    return isIdenticalStmt(Ctx, ReturnStmt1->getRetValue(),
                           ReturnStmt2->getRetValue(), IgnoreSideEffects);
  }
  case Stmt::ForStmtClass: {
    const ForStmt *ForStmt1 = cast<ForStmt>(Stmt1);
    const ForStmt *ForStmt2 = cast<ForStmt>(Stmt2);

    if (!isIdenticalStmt(Ctx, ForStmt1->getInit(), ForStmt2->getInit(),
                         IgnoreSideEffects))
      return false;
    if (!isIdenticalStmt(Ctx, ForStmt1->getCond(), ForStmt2->getCond(),
                         IgnoreSideEffects))
      return false;
    if (!isIdenticalStmt(Ctx, ForStmt1->getInc(), ForStmt2->getInc(),
                         IgnoreSideEffects))
      return false;
    if (!isIdenticalStmt(Ctx, ForStmt1->getBody(), ForStmt2->getBody(),
                         IgnoreSideEffects))
      return false;
    return true;
  }
  case Stmt::DoStmtClass: {
    const DoStmt *DStmt1 = cast<DoStmt>(Stmt1);
    const DoStmt *DStmt2 = cast<DoStmt>(Stmt2);

    if (!isIdenticalStmt(Ctx, DStmt1->getCond(), DStmt2->getCond(),
                         IgnoreSideEffects))
      return false;
    if (!isIdenticalStmt(Ctx, DStmt1->getBody(), DStmt2->getBody(),
                         IgnoreSideEffects))
      return false;
    return true;
  }
  case Stmt::WhileStmtClass: {
    const WhileStmt *WStmt1 = cast<WhileStmt>(Stmt1);
    const WhileStmt *WStmt2 = cast<WhileStmt>(Stmt2);

    if (!isIdenticalStmt(Ctx, WStmt1->getCond(), WStmt2->getCond(),
                         IgnoreSideEffects))
      return false;
    if (!isIdenticalStmt(Ctx, WStmt1->getBody(), WStmt2->getBody(),
                         IgnoreSideEffects))
      return false;
    return true;
  }
  case Stmt::IfStmtClass: {
    const IfStmt *IStmt1 = cast<IfStmt>(Stmt1);
    const IfStmt *IStmt2 = cast<IfStmt>(Stmt2);

    if (!isIdenticalStmt(Ctx, IStmt1->getCond(), IStmt2->getCond(),
                         IgnoreSideEffects))
      return false;
    if (!isIdenticalStmt(Ctx, IStmt1->getThen(), IStmt2->getThen(),
                         IgnoreSideEffects))
      return false;
    if (!isIdenticalStmt(Ctx, IStmt1->getElse(), IStmt2->getElse(),
                         IgnoreSideEffects))
      return false;
    return true;
  }
  case Stmt::CompoundStmtClass: {
    const CompoundStmt *CompStmt1 = cast<CompoundStmt>(Stmt1);
    const CompoundStmt *CompStmt2 = cast<CompoundStmt>(Stmt2);

    if (CompStmt1->size() != CompStmt2->size())
      return false;

    CompoundStmt::const_body_iterator I1 = CompStmt1->body_begin();
    CompoundStmt::const_body_iterator I2 = CompStmt2->body_begin();
    while (I1 != CompStmt1->body_end() && I2 != CompStmt2->body_end()) {
      if (!isIdenticalStmt(Ctx, *I1, *I2, IgnoreSideEffects))
        return false;
      ++I1;
      ++I2;
    }

    return true;
  }
  case Stmt::CompoundAssignOperatorClass:
  case Stmt::BinaryOperatorClass: {
    const BinaryOperator *BinOp1 = cast<BinaryOperator>(Stmt1);
    const BinaryOperator *BinOp2 = cast<BinaryOperator>(Stmt2);
    return BinOp1->getOpcode() == BinOp2->getOpcode();
  }
  case Stmt::CharacterLiteralClass: {
    const CharacterLiteral *CharLit1 = cast<CharacterLiteral>(Stmt1);
    const CharacterLiteral *CharLit2 = cast<CharacterLiteral>(Stmt2);
    return CharLit1->getValue() == CharLit2->getValue();
  }
  case Stmt::DeclRefExprClass: {
    const DeclRefExpr *DeclRef1 = cast<DeclRefExpr>(Stmt1);
    const DeclRefExpr *DeclRef2 = cast<DeclRefExpr>(Stmt2);
    return DeclRef1->getDecl() == DeclRef2->getDecl();
  }
  case Stmt::IntegerLiteralClass: {
    const IntegerLiteral *IntLit1 = cast<IntegerLiteral>(Stmt1);
    const IntegerLiteral *IntLit2 = cast<IntegerLiteral>(Stmt2);

    llvm::APInt I1 = IntLit1->getValue();
    llvm::APInt I2 = IntLit2->getValue();
    if (I1.getBitWidth() != I2.getBitWidth())
      return false;
    return I1 == I2;
  }
  case Stmt::FloatingLiteralClass: {
    const FloatingLiteral *FloatLit1 = cast<FloatingLiteral>(Stmt1);
    const FloatingLiteral *FloatLit2 = cast<FloatingLiteral>(Stmt2);
    return FloatLit1->getValue().bitwiseIsEqual(FloatLit2->getValue());
  }
  case Stmt::StringLiteralClass: {
    const StringLiteral *StringLit1 = cast<StringLiteral>(Stmt1);
    const StringLiteral *StringLit2 = cast<StringLiteral>(Stmt2);
    return StringLit1->getBytes() == StringLit2->getBytes();
  }
  case Stmt::MemberExprClass: {
    const MemberExpr *MemberStmt1 = cast<MemberExpr>(Stmt1);
    const MemberExpr *MemberStmt2 = cast<MemberExpr>(Stmt2);
    return MemberStmt1->getMemberDecl() == MemberStmt2->getMemberDecl();
  }
  case Stmt::UnaryOperatorClass: {
    const UnaryOperator *UnaryOp1 = cast<UnaryOperator>(Stmt1);
    const UnaryOperator *UnaryOp2 = cast<UnaryOperator>(Stmt2);
    return UnaryOp1->getOpcode() == UnaryOp2->getOpcode();
  }
  }
}
