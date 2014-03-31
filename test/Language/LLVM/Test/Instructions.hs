{-# LANGUAGE  QuasiQuotes #-}
module Language.LLVM.Test.Instructions where

import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit

import Control.Monad
import Data.Functor
import Data.Maybe
import Foreign.Ptr
import Data.Word

import Language.LLVM.Quote.LLVM

import LLVM.General.AST
import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.IntegerPredicate as IPred
import qualified LLVM.General.AST.FloatingPointPredicate as FPPred
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.RMWOperation as RMWOp

tests = let a = LocalReference . UnName in testGroup "Instructions" [
  testGroup "regular" [
    testCase name $ instrQ @?= instr
    | (name, instr, instrQ) <- [
          ("add",
           Add {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|add i32 %0, %0|]),
          ("nsw",
           Add {
             nsw = True,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|add nsw i32 %0, %0|]),
          ("nuw",
           Add {
             nsw = False,
             nuw = True,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|add nuw i32 %0, %0|]),
          ("fadd",
           FAdd {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           [lli|fadd float %1, %1|]),
          ("sub",
           Sub {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|sub i32 %0, %0|]),
          ("fsub",
           FSub {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           [lli|fsub float %1, %1|]),
          ("mul",
           Mul {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|mul i32 %0, %0|]),
          ("fmul",
           FMul {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           [lli|fmul float %1, %1|]),
          ("udiv",
           UDiv {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|udiv i32 %0, %0|]),
          ("exact",
           UDiv {
             exact = True,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|udiv exact i32 %0, %0|]),
          ("sdiv",
           SDiv {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|sdiv i32 %0, %0|]),
          ("fdiv",
           FDiv {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           [lli|fdiv float %1, %1|]),
          ("urem",
           URem {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|urem i32 %0, %0|]),
          ("srem",
           SRem {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|srem i32 %0, %0|]),
          ("frem",
           FRem {
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           [lli|frem float %1, %1|]),
          ("shl",
           Shl {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|shl i32 %0, %0|]),
          ("ashr",
           AShr {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|ashr i32 %0, %0|]),
          ("lshr",
           LShr {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|lshr i32 %0, %0|]),
          ("and",
           And {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|and i32 %0, %0|]),
          ("or",
           Or {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|or i32 %0, %0|]),
          ("xor",
           Xor {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           [lli|xor i32 %0, %0|]),
          ("alloca",
           Alloca {
             allocatedType = IntegerType 32,
             numElements = Nothing,
             alignment = 0,
             metadata = [] 
           },
           [lli|alloca i32|]),
          ("alloca tricky",
           Alloca {
             allocatedType = IntegerType 7,
             numElements = Just (ConstantOperand (C.Int 32 2)),
             alignment = 128,
             metadata = [] 
           },
           [lli|alloca i7, i32 2, align 128|]),
          ("load",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Nothing,
             alignment = 0,
             metadata = [] 
           },
           [lli|load i32* %2|]),
          ("volatile",
           Load {
             volatile = True,
             address = a 2,
             maybeAtomicity = Nothing,
             alignment = 0,
             metadata = [] 
           },
           [lli|load volatile i32* %2|]),
          ("acquire",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Just (Atomicity { crossThread = True, memoryOrdering = Acquire }),
             alignment = 1,
             metadata = [] 
           },
           [lli|load atomic i32* %2 acquire, align 1|]),
          ("singlethread",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Just (Atomicity { crossThread = False, memoryOrdering = Monotonic }),
             alignment = 1,
             metadata = [] 
           },
           [lli|load atomic i32* %2 singlethread monotonic, align 1|]),
          ("GEP",
           GetElementPtr {
             inBounds = False,
             address = a 2,
             indices = [ a 0 ],
             metadata = [] 
           },
           [lli|getelementptr i32* %2, i32 %0|]),
          ("inBounds",
           GetElementPtr {
             inBounds = True,
             address = a 2,
             indices = [ a 0 ],
             metadata = [] 
           },
           [lli|getelementptr inbounds i32* %2, i32 %0|]),
          ("cmpxchg",
           CmpXchg {
             volatile = False,
             address = a 2,
             expected = a 0,
             replacement = a 0,
             atomicity = Atomicity { crossThread = True, memoryOrdering = Monotonic },
             metadata = [] 
           },
           [lli|cmpxchg i32* %2, i32 %0, i32 %0 monotonic|]),
          ("atomicrmw",
           AtomicRMW {
             volatile = False,
             rmwOperation = RMWOp.UMax,
             address = a 2,
             value = a 0,
             atomicity = Atomicity { crossThread = True, memoryOrdering = Release },
             metadata = []
           },
           [lli|atomicrmw umax i32* %2, i32 %0 release|]),

          ("trunc",
           Trunc {
             operand0 = a 0,
             type' = IntegerType 16,
             metadata = [] 
           },
           [lli|trunc i32 %0 to i16|]),
          ("zext",
           ZExt {
             operand0 = a 0,
             type' = IntegerType 64,
             metadata = [] 
           },
           [lli|zext i32 %0 to i64|]),
          ("sext",
           SExt {
             operand0 = a 0,
             type' = IntegerType 64,
             metadata = [] 
           },
           [lli|sext i32 %0 to i64|]),
          ("fptoui",
           FPToUI {
             operand0 = a 1,
             type' = IntegerType 64,
             metadata = [] 
           },
           [lli|fptoui float %1 to i64|]),
          ("fptosi",
           FPToSI {
             operand0 = a 1,
             type' = IntegerType 64,
             metadata = [] 
           },
           [lli|fptosi float %1 to i64|]),
          ("uitofp",
           UIToFP {
             operand0 = a 0,
             type' = FloatingPointType 32 IEEE,
             metadata = [] 
           },
           [lli|uitofp i32 %0 to float|]),
          ("sitofp",
           SIToFP {
             operand0 = a 0,
             type' = FloatingPointType 32 IEEE,
             metadata = [] 
           },
           [lli|sitofp i32 %0 to float|]),
          ("fptrunc",
           FPTrunc {
             operand0 = a 1,
             type' = FloatingPointType 16 IEEE,
             metadata = [] 
           },
           [lli|fptrunc float %1 to half|]),
          ("fpext",
           FPExt {
             operand0 = a 1,
             type' = FloatingPointType 64 IEEE,
             metadata = [] 
           },
           [lli|fpext float %1 to double|]),
          ("ptrtoint",
           PtrToInt {
             operand0 = a 2,
             type' = IntegerType 32,
             metadata = [] 
           },
           [lli|ptrtoint i32* %2 to i32|]),
          ("inttoptr",
           IntToPtr {
             operand0 = a 0,
             type' = PointerType (IntegerType 32) (AddrSpace 0),
             metadata = [] 
           },
           [lli|inttoptr i32 %0 to i32*|]),
          ("bitcast",
           BitCast {
             operand0 = a 0,
             type' = FloatingPointType 32 IEEE,
             metadata = [] 
           },
           [lli|bitcast i32 %0 to float|]),
          ("addrspacecast",
           AddrSpaceCast {
             operand0 = a 2,
             type' = PointerType (IntegerType 32) (AddrSpace 2),
             metadata = [] 
           },
           [lli|addrspacecast i32* %2 to i32 addrspace(2)*|]),
          ("select",
           Select {
             condition' = a 4,
             trueValue = a 0,
             falseValue = a 0,
             metadata = []
           },
           [lli|select i1 %4, i32 %0, i32 %0|]),
          ("vaarg",
           VAArg {
             argList = a 2,
             type' = IntegerType 16,
             metadata = []
           },
           [lli|va_arg i32* %2, i16|]),
          ("extractelement",
           ExtractElement {
             vector = a 5,
             index = a 0,
             metadata = []
           },
           [lli|extractelement <2 x i32> %5, i32 %0|]),
          ("insertelement",
           InsertElement {
             vector = a 5,
             element = a 0,
             index = a 0,
             metadata = []
           },
           [lli|insertelement <2 x i32> %5, i32 %0, i32 %0|]),
          ("shufflevector",
           ShuffleVector {
             operand0 = a 5,
             operand1 = a 5,
             mask = C.Vector [ C.Int 32 p | p <- [0..1] ],
             metadata = []
           },
           [lli|shufflevector <2 x i32> %5, <2 x i32> %5, <2 x i32> <i32 0, i32 1>|]),
          ("extractvalue",
           ExtractValue {
             aggregate = a 6,
             indices' = [0],
             metadata = []
           },
           [lli|extractvalue { i32, i32 } %6, 0|]),
          ("insertvalue",
           InsertValue {
             aggregate = a 6,
             element = a 0,
             indices' = [0],
             metadata = []
           },
           [lli|insertvalue { i32, i32 } %6, i32 %0, 0|])
         ]
   ],

   testGroup "terminators" [
    testCase name $ mASTQ @?= mAST
    | (name, mAST, mASTQ) <- [
     (
       "ret",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "entry") [
             ] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       [llmod|; ModuleID = '<string>'
       
       define void @0() {
       entry:
         ret void
       }|]
     ), (
       "br",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "entry") [] (
              Do $ Br (Name "foo") []
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       [llmod|; ModuleID = '<string>'
       
       define void @0() {
       entry:
         br label %foo
       
       foo:                                              ; preds = %0
         ret void
       }|]
     ), (
       "condbr",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "bar") [] (
              Do $ CondBr (ConstantOperand (C.Int 1 1)) (Name "foo") (Name "bar") []
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             )
           ]
          }
        ],
       [llmod|; ModuleID = '<string>'
       
       define void @0() {
       bar:
         br i1 true, label %foo, label %bar
       
       foo:                                              ; preds = %bar
         ret void
       }|]
     ), (
       "switch",
       Module "<string>" Nothing Nothing [
         GlobalDefinition $ functionDefaults {
           G.returnType = VoidType,
           G.name = UnName 0,
           G.basicBlocks = [
             BasicBlock (Name "entry") [] (
               Do $ Switch {
                 operand0' = ConstantOperand (C.Int 16 2),
                 defaultDest = Name "foo",
                 dests = [
                  (C.Int 16 0, Name "entry"),
                  (C.Int 16 2, Name "foo"),
                  (C.Int 16 3, Name "entry")
                 ],
                 metadata' = []
              }
             ),
             BasicBlock (Name "foo") [] (
               Do $ Ret Nothing []
              )
            ]
          }
        ],
       [llmod|; ModuleID = '<string>'
       
       define void @0() {
       entry:
         switch i16 2, label %foo [
           i16 0, label %entry
           i16 2, label %foo
           i16 3, label %entry
         ]
       
       foo:
         ret void
       }|]
     ), (
       "indirectbr",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ globalVariableDefaults {
          G.name = UnName 0,
          G.type' = PointerType (IntegerType 8) (AddrSpace 0),
          G.initializer = Just (C.BlockAddress (Name "foo") (Name "end"))
        },
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = Name "foo",
          G.basicBlocks = [
            BasicBlock (Name "entry") [
              UnName 0 := Load {
                       volatile = False,
                       address = ConstantOperand (C.GlobalReference (UnName 0)),
                       maybeAtomicity = Nothing,
                       alignment = 0,
                       metadata = [] 
                     }
            ] (
              Do $ IndirectBr {
                operand0' = LocalReference (UnName 0),
                possibleDests = [Name "end"],
                metadata' = []
             }
            ),
            BasicBlock (Name "end") [] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
--       \  indirectbr i8* null, [label %foo]\n\
       [llmod|; ModuleID = '<string>'
       
       @0 = global i8* blockaddress(@foo, %end)
       
       define void @foo() {
       entry:
         %0 = load i8** @0
         indirectbr i8* %0, [label %end]
       
       end:
         ret void
       }|]
     ), (
       "invoke",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.parameters = ([
            Parameter (IntegerType 32) (Name "a") [],
            Parameter (IntegerType 16) (Name "b") []
           ], False),
          G.basicBlocks = [
            BasicBlock (Name "entry") [] (
              Do $ Invoke {
               callingConvention' = CC.C,
               returnAttributes' = [],
               function' = Right (ConstantOperand (C.GlobalReference (UnName 0))),
               arguments' = [
                (ConstantOperand (C.Int 32 4), []),
                (ConstantOperand (C.Int 16 8), [])
               ],
               functionAttributes' = [],
               returnDest = Name "foo",
               exceptionDest = Name "bar",
               metadata' = []
              }
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             ),
            BasicBlock (Name "bar") [
             UnName 0 := LandingPad {
               type' = StructureType False [ 
                  PointerType (IntegerType 8) (AddrSpace 0),
                  IntegerType 32
                 ],
               personalityFunction = ConstantOperand (C.GlobalReference (UnName 0)),
               cleanup = True,
               clauses = [Catch (C.Null (PointerType (IntegerType 8) (AddrSpace 0)))],
               metadata = []
             }
             ] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       [llmod|; ModuleID = '<string>'
       
       define void @0(i32 %a, i16 %b) {
       entry:
         invoke void @0(i32 4, i16 8)
                 to label %foo unwind label %bar
       
       foo:                                       
         ret void
       
       bar:                                       
         %0 = landingpad { i8*, i32 } personality void (i32, i16)* @0
                 cleanup
                 catch i8* null
         ret void
       }|]
     ), (
       "resume",
       Module "<string>" Nothing Nothing [
         GlobalDefinition $ functionDefaults {
           G.returnType = VoidType,
           G.name = UnName 0,
           G.basicBlocks = [
             BasicBlock (Name "entry") [] (
               Do $ Resume (ConstantOperand (C.Int 32 1)) []
              )
            ]
          }
        ],
       [llmod|; ModuleID = '<string>'
       
       define void @0() {
       entry:
         resume i32 1
       }|]
     ), (
       "unreachable",
       Module "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "entry") [] (
              Do $ Unreachable []
             )
           ]
         }
        ],
       [llmod|; ModuleID = '<string>'
       
       define void @0() {
       entry:
         unreachable
       }|]
     )
    ]
   ]
 ]