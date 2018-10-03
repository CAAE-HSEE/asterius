{-# LANGUAGE TypeApplications #-}

module Language.WebAssembly.WireFormat.Orphans
  ( genSection
  ) where

import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.Word
import Language.WebAssembly.WireFormat
import Test.QuickCheck.Gen

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [pure Nothing, Just <$> g]

genSBS :: Gen SBS.ShortByteString
genSBS = SBS.pack <$> listOf chooseAny

genName :: Gen Name
genName = coerce genSBS

genValueType :: Gen ValueType
genValueType = Test.QuickCheck.Gen.elements [I32, I64, F32, F64]

genResultType :: Gen [ValueType]
genResultType = do
  n <- choose (0, 1)
  vectorOf n genValueType

genFunctionType :: Gen FunctionType
genFunctionType = FunctionType <$> listOf genValueType <*> genResultType

genLimits :: Gen Limits
genLimits = Limits <$> chooseAny <*> genMaybe chooseAny

genMemoryType :: Gen MemoryType
genMemoryType = coerce genLimits

genElementType :: Gen ElementType
genElementType = pure AnyFunc

genTableType :: Gen TableType
genTableType = TableType <$> genElementType <*> genLimits

genMutability :: Gen Mutability
genMutability = Test.QuickCheck.Gen.elements [Const, Var]

genGlobalType :: Gen GlobalType
genGlobalType = GlobalType <$> genValueType <*> genMutability

genMemoryArgument :: Gen MemoryArgument
genMemoryArgument = MemoryArgument <$> chooseAny <*> chooseAny

genInstructions :: Gen [Instruction]
genInstructions = listOf genInstruction

genInstruction :: Gen Instruction
genInstruction =
  oneof
    [ pure Unreachable
    , pure Nop
    , Block <$> genResultType <*> genInstructions
    , Loop <$> genResultType <*> genInstructions
    , If <$> genResultType <*> genInstructions <*> genMaybe genInstructions
    , Branch <$> genLabelIndex
    , BranchIf <$> genLabelIndex
    , BranchTable <$> listOf genLabelIndex <*> genLabelIndex
    , pure Return
    , Call <$> genFunctionIndex
    , CallIndirect <$> genFunctionTypeIndex
    , pure Drop
    , pure Select
    , GetLocal <$> genLocalIndex
    , SetLocal <$> genLocalIndex
    , TeeLocal <$> genLocalIndex
    , GetGlobal <$> genGlobalIndex
    , SetGlobal <$> genGlobalIndex
    , I32Load <$> genMemoryArgument
    , I64Load <$> genMemoryArgument
    , F32Load <$> genMemoryArgument
    , F64Load <$> genMemoryArgument
    , I32Load8Signed <$> genMemoryArgument
    , I32Load8Unsigned <$> genMemoryArgument
    , I32Load16Signed <$> genMemoryArgument
    , I32Load16Unsigned <$> genMemoryArgument
    , I64Load8Signed <$> genMemoryArgument
    , I64Load8Unsigned <$> genMemoryArgument
    , I64Load16Signed <$> genMemoryArgument
    , I64Load16Unsigned <$> genMemoryArgument
    , I64Load32Signed <$> genMemoryArgument
    , I64Load32Unsigned <$> genMemoryArgument
    , I32Store <$> genMemoryArgument
    , I64Store <$> genMemoryArgument
    , F32Store <$> genMemoryArgument
    , F64Store <$> genMemoryArgument
    , I32Store8 <$> genMemoryArgument
    , I32Store16 <$> genMemoryArgument
    , I64Store8 <$> genMemoryArgument
    , I64Store16 <$> genMemoryArgument
    , I64Store32 <$> genMemoryArgument
    , pure MemorySize
    , pure MemoryGrow
    , pure I32Eqz
    , pure I32Eq
    , pure I32Ne
    , pure I32LtS
    , pure I32LtU
    , pure I32GtS
    , pure I32GtU
    , pure I32LeS
    , pure I32LeU
    , pure I32GeS
    , pure I32GeU
    , pure I64Eqz
    , pure I64Eq
    , pure I64Ne
    , pure I64LtS
    , pure I64LtU
    , pure I64GtS
    , pure I64GtU
    , pure I64LeS
    , pure I64LeU
    , pure I64GeS
    , pure I64GeU
    , pure F32Eq
    , pure F32Ne
    , pure F32Lt
    , pure F32Gt
    , pure F32Le
    , pure F32Ge
    , pure F64Eq
    , pure F64Ne
    , pure F64Lt
    , pure F64Gt
    , pure F64Le
    , pure F64Ge
    , pure I32Clz
    , pure I32Ctz
    , pure I32Popcnt
    , pure I32Add
    , pure I32Sub
    , pure I32Mul
    , pure I32DivS
    , pure I32DivU
    , pure I32RemS
    , pure I32RemU
    , pure I32And
    , pure I32Or
    , pure I32Xor
    , pure I32Shl
    , pure I32ShrS
    , pure I32ShrU
    , pure I32RotL
    , pure I32RotR
    , pure I64Clz
    , pure I64Ctz
    , pure I64Popcnt
    , pure I64Add
    , pure I64Sub
    , pure I64Mul
    , pure I64DivS
    , pure I64DivU
    , pure I64RemS
    , pure I64RemU
    , pure I64And
    , pure I64Or
    , pure I64Xor
    , pure I64Shl
    , pure I64ShrS
    , pure I64ShrU
    , pure I64RotL
    , pure I64RotR
    , pure F32Abs
    , pure F32Neg
    , pure F32Ceil
    , pure F32Floor
    , pure F32Trunc
    , pure F32Nearest
    , pure F32Sqrt
    , pure F32Add
    , pure F32Sub
    , pure F32Mul
    , pure F32Div
    , pure F32Min
    , pure F32Max
    , pure F32Copysign
    , pure F64Abs
    , pure F64Neg
    , pure F64Ceil
    , pure F64Floor
    , pure F64Trunc
    , pure F64Nearest
    , pure F64Sqrt
    , pure F64Add
    , pure F64Sub
    , pure F64Mul
    , pure F64Div
    , pure F64Min
    , pure F64Max
    , pure F64Copysign
    , pure I32WrapFromI64
    , pure I32TruncSFromF32
    , pure I32TruncUFromF32
    , pure I32TruncSFromF64
    , pure I32TruncUFromF64
    , pure I64ExtendSFromI32
    , pure I64ExtendUFromI32
    , pure I64TruncSFromF32
    , pure I64TruncUFromF32
    , pure I64TruncSFromF64
    , pure I64TruncUFromF64
    , pure F32ConvertSFromI32
    , pure F32ConvertUFromI32
    , pure F32ConvertSFromI64
    , pure F32ConvertUFromI64
    , pure F32DemoteFromF64
    , pure F64ConvertSFromI32
    , pure F64ConvertUFromI32
    , pure F64ConvertSFromI64
    , pure F64ConvertUFromI64
    , pure F64PromoteFromF32
    , pure I32ReinterpretFromF32
    , pure I64ReinterpretFromF64
    , pure F32ReinterpretFromI32
    , pure F64ReinterpretFromI64
    ]

genExpression :: Gen Expression
genExpression = Expression <$> listOf genInstruction

genCustom :: Gen Custom
genCustom = Custom <$> genName <*> genSBS

genFunctionTypeIndex :: Gen FunctionTypeIndex
genFunctionTypeIndex = coerce (chooseAny @Word32)

genFunctionIndex :: Gen FunctionIndex
genFunctionIndex = coerce (chooseAny @Word32)

genTableIndex :: Gen TableIndex
genTableIndex = coerce (chooseAny @Word32)

genMemoryIndex :: Gen MemoryIndex
genMemoryIndex = coerce (chooseAny @Word32)

genGlobalIndex :: Gen GlobalIndex
genGlobalIndex = coerce (chooseAny @Word32)

genLocalIndex :: Gen LocalIndex
genLocalIndex = coerce (chooseAny @Word32)

genLabelIndex :: Gen LabelIndex
genLabelIndex = coerce (chooseAny @Word32)

genImportDescription :: Gen ImportDescription
genImportDescription =
  oneof
    [ ImportFunction <$> genFunctionTypeIndex
    , ImportTable <$> genTableType
    , ImportMemory <$> genMemoryType
    , ImportGlobal <$> genGlobalType
    ]

genImport :: Gen Import
genImport = Import <$> genName <*> genName <*> genImportDescription

genTable :: Gen Table
genTable = coerce genTableType

genMemory :: Gen Memory
genMemory = coerce genMemoryType

genGlobal :: Gen Global
genGlobal = Global <$> genGlobalType <*> genExpression

genExportDescription :: Gen ExportDescription
genExportDescription =
  oneof
    [ ExportFunction <$> genFunctionIndex
    , ExportTable <$> genTableIndex
    , ExportMemory <$> genMemoryIndex
    , ExportGlobal <$> genGlobalIndex
    ]

genExport :: Gen Export
genExport = Export <$> genName <*> genExportDescription

genStart :: Gen Start
genStart = coerce genFunctionIndex

genElement :: Gen Element
genElement =
  Element <$> genTableIndex <*> genExpression <*> listOf genFunctionIndex

genLocals :: Gen Locals
genLocals = Locals <$> chooseAny <*> genValueType

genFunction :: Gen Function
genFunction = Function <$> listOf genLocals <*> genExpression

genData :: Gen Data
genData = Data <$> genMemoryIndex <*> genExpression <*> genSBS

genSection :: Gen Section
genSection =
  oneof
    [ CustomSection <$> genCustom
    , TypeSection <$> listOf genFunctionType
    , ImportSection <$> listOf genImport
    , FunctionSection <$> listOf genFunctionTypeIndex
    , TableSection <$> listOf genTable
    , MemorySection <$> listOf genMemory
    , GlobalSection <$> listOf genGlobal
    , ExportSection <$> listOf genExport
    , StartSection <$> genStart
    , ElementSection <$> listOf genElement
    , CodeSection <$> listOf genFunction
    , DataSection <$> listOf genData
    ]
