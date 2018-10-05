{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.WebAssembly.WireFormat.Orphans
  ( genModule
  ) where

import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.Word
import Language.WebAssembly.WireFormat
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [pure Nothing, Just <$> g]

genSBS :: Gen SBS.ShortByteString
genSBS = SBS.pack <$> listOf chooseAny

instance Arbitrary SBS.ShortByteString where
  arbitrary = genSBS
  shrink = map SBS.pack . shrink . SBS.unpack

genName :: Gen Name
genName = coerce genSBS

instance Arbitrary Name where
  arbitrary = genName
  shrink = genericShrink

genValueType :: Gen ValueType
genValueType = Test.QuickCheck.Gen.elements [I32, I64, F32, F64]

instance Arbitrary ValueType where
  arbitrary = genValueType
  shrink = genericShrink

genResultType :: Gen [ValueType]
genResultType = do
  n <- choose (0, 1)
  vectorOf n genValueType

genFunctionType :: Gen FunctionType
genFunctionType = FunctionType <$> listOf genValueType <*> genResultType

instance Arbitrary FunctionType where
  arbitrary = genFunctionType
  shrink = genericShrink

genLimits :: Gen Limits
genLimits = Limits <$> chooseAny <*> genMaybe chooseAny

instance Arbitrary Limits where
  arbitrary = genLimits
  shrink = genericShrink

genMemoryType :: Gen MemoryType
genMemoryType = coerce genLimits

instance Arbitrary MemoryType where
  arbitrary = genMemoryType
  shrink = genericShrink

genElementType :: Gen ElementType
genElementType = pure AnyFunc

instance Arbitrary ElementType where
  arbitrary = genElementType
  shrink = genericShrink

genTableType :: Gen TableType
genTableType = TableType <$> genElementType <*> genLimits

instance Arbitrary TableType where
  arbitrary = genTableType
  shrink = genericShrink

genMutability :: Gen Mutability
genMutability = Test.QuickCheck.Gen.elements [Const, Var]

instance Arbitrary Mutability where
  arbitrary = genMutability
  shrink = genericShrink

genGlobalType :: Gen GlobalType
genGlobalType = GlobalType <$> genValueType <*> genMutability

instance Arbitrary GlobalType where
  arbitrary = genGlobalType
  shrink = genericShrink

genMemoryArgument :: Gen MemoryArgument
genMemoryArgument = MemoryArgument <$> chooseAny <*> chooseAny

instance Arbitrary MemoryArgument where
  arbitrary = genMemoryArgument
  shrink = genericShrink

genUnaryOperator :: Gen UnaryOperator
genUnaryOperator =
  Test.QuickCheck.Gen.elements
    [ I32Eqz
    , I64Eqz
    , I32Clz
    , I32Ctz
    , I32Popcnt
    , I64Clz
    , I64Ctz
    , I64Popcnt
    , F32Abs
    , F32Neg
    , F32Ceil
    , F32Floor
    , F32Trunc
    , F32Nearest
    , F32Sqrt
    , F64Abs
    , F64Neg
    , F64Ceil
    , F64Floor
    , F64Trunc
    , F64Nearest
    , F64Sqrt
    , I32WrapFromI64
    , I32TruncSFromF32
    , I32TruncUFromF32
    , I32TruncSFromF64
    , I32TruncUFromF64
    , I64ExtendSFromI32
    , I64ExtendUFromI32
    , I64TruncSFromF32
    , I64TruncUFromF32
    , I64TruncSFromF64
    , I64TruncUFromF64
    , F32ConvertSFromI32
    , F32ConvertUFromI32
    , F32ConvertSFromI64
    , F32ConvertUFromI64
    , F32DemoteFromF64
    , F64ConvertSFromI32
    , F64ConvertUFromI32
    , F64ConvertSFromI64
    , F64ConvertUFromI64
    , F64PromoteFromF32
    , I32ReinterpretFromF32
    , I64ReinterpretFromF64
    , F32ReinterpretFromI32
    , F64ReinterpretFromI64
    ]

instance Arbitrary UnaryOperator where
  arbitrary = genUnaryOperator
  shrink = genericShrink

genBinaryOperator :: Gen BinaryOperator
genBinaryOperator =
  Test.QuickCheck.Gen.elements
    [ I32Eq
    , I32Ne
    , I32LtS
    , I32LtU
    , I32GtS
    , I32GtU
    , I32LeS
    , I32LeU
    , I32GeS
    , I32GeU
    , I64Eq
    , I64Ne
    , I64LtS
    , I64LtU
    , I64GtS
    , I64GtU
    , I64LeS
    , I64LeU
    , I64GeS
    , I64GeU
    , F32Eq
    , F32Ne
    , F32Lt
    , F32Gt
    , F32Le
    , F32Ge
    , F64Eq
    , F64Ne
    , F64Lt
    , F64Gt
    , F64Le
    , F64Ge
    , I32Add
    , I32Sub
    , I32Mul
    , I32DivS
    , I32DivU
    , I32RemS
    , I32RemU
    , I32And
    , I32Or
    , I32Xor
    , I32Shl
    , I32ShrS
    , I32ShrU
    , I32RotL
    , I32RotR
    , I64Add
    , I64Sub
    , I64Mul
    , I64DivS
    , I64DivU
    , I64RemS
    , I64RemU
    , I64And
    , I64Or
    , I64Xor
    , I64Shl
    , I64ShrS
    , I64ShrU
    , I64RotL
    , I64RotR
    , F32Add
    , F32Sub
    , F32Mul
    , F32Div
    , F32Min
    , F32Max
    , F32Copysign
    , F64Add
    , F64Sub
    , F64Mul
    , F64Div
    , F64Min
    , F64Max
    , F64Copysign
    ]

instance Arbitrary BinaryOperator where
  arbitrary = genBinaryOperator
  shrink = genericShrink

genInstructions :: Gen [Instruction]
genInstructions = listOf genInstruction

genInstruction :: Gen Instruction
genInstruction =
  frequency
    [ ( 64
      , oneof
          [ UnaryInstruction <$> genUnaryOperator
          , BinaryInstruction <$> genBinaryOperator
          , pure Unreachable
          , pure Nop
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
          ])
    , ( 1
      , oneof
          [ Block <$> genResultType <*> genInstructions
          , Loop <$> genResultType <*> genInstructions
          , If <$> genResultType <*> genInstructions <*>
            genMaybe genInstructions
          ])
    ]

instance Arbitrary Instruction where
  arbitrary = genInstruction
  shrink = genericShrink

genExpression :: Gen Expression
genExpression = Expression <$> listOf genInstruction

instance Arbitrary Expression where
  arbitrary = genExpression
  shrink = genericShrink

genCustom :: Gen Custom
genCustom = Custom <$> genName <*> genSBS

instance Arbitrary Custom where
  arbitrary = genCustom
  shrink = genericShrink

genFunctionTypeIndex :: Gen FunctionTypeIndex
genFunctionTypeIndex = coerce (chooseAny @Word32)

instance Arbitrary FunctionTypeIndex where
  arbitrary = genFunctionTypeIndex
  shrink = genericShrink

genFunctionIndex :: Gen FunctionIndex
genFunctionIndex = coerce (chooseAny @Word32)

instance Arbitrary FunctionIndex where
  arbitrary = genFunctionIndex
  shrink = genericShrink

genTableIndex :: Gen TableIndex
genTableIndex = coerce (chooseAny @Word32)

instance Arbitrary TableIndex where
  arbitrary = genTableIndex
  shrink = genericShrink

genMemoryIndex :: Gen MemoryIndex
genMemoryIndex = coerce (chooseAny @Word32)

instance Arbitrary MemoryIndex where
  arbitrary = genMemoryIndex
  shrink = genericShrink

genGlobalIndex :: Gen GlobalIndex
genGlobalIndex = coerce (chooseAny @Word32)

instance Arbitrary GlobalIndex where
  arbitrary = genGlobalIndex
  shrink = genericShrink

genLocalIndex :: Gen LocalIndex
genLocalIndex = coerce (chooseAny @Word32)

instance Arbitrary LocalIndex where
  arbitrary = genLocalIndex
  shrink = genericShrink

genLabelIndex :: Gen LabelIndex
genLabelIndex = coerce (chooseAny @Word32)

instance Arbitrary LabelIndex where
  arbitrary = genLabelIndex
  shrink = genericShrink

genImportDescription :: Gen ImportDescription
genImportDescription =
  oneof
    [ ImportFunction <$> genFunctionTypeIndex
    , ImportTable <$> genTableType
    , ImportMemory <$> genMemoryType
    , ImportGlobal <$> genGlobalType
    ]

instance Arbitrary ImportDescription where
  arbitrary = genImportDescription
  shrink = genericShrink

genImport :: Gen Import
genImport = Import <$> genName <*> genName <*> genImportDescription

instance Arbitrary Import where
  arbitrary = genImport
  shrink = genericShrink

genTable :: Gen Table
genTable = coerce genTableType

instance Arbitrary Table where
  arbitrary = genTable
  shrink = genericShrink

genMemory :: Gen Memory
genMemory = coerce genMemoryType

instance Arbitrary Memory where
  arbitrary = genMemory
  shrink = genericShrink

genGlobal :: Gen Global
genGlobal = Global <$> genGlobalType <*> genExpression

instance Arbitrary Global where
  arbitrary = genGlobal
  shrink = genericShrink

genExportDescription :: Gen ExportDescription
genExportDescription =
  oneof
    [ ExportFunction <$> genFunctionIndex
    , ExportTable <$> genTableIndex
    , ExportMemory <$> genMemoryIndex
    , ExportGlobal <$> genGlobalIndex
    ]

instance Arbitrary ExportDescription where
  arbitrary = genExportDescription
  shrink = genericShrink

genExport :: Gen Export
genExport = Export <$> genName <*> genExportDescription

instance Arbitrary Export where
  arbitrary = genExport
  shrink = genericShrink

genStart :: Gen Start
genStart = coerce genFunctionIndex

instance Arbitrary Start where
  arbitrary = genStart
  shrink = genericShrink

genElement :: Gen Element
genElement =
  Element <$> genTableIndex <*> genExpression <*> listOf genFunctionIndex

instance Arbitrary Element where
  arbitrary = genElement
  shrink = genericShrink

genLocals :: Gen Locals
genLocals = Locals <$> chooseAny <*> genValueType

instance Arbitrary Locals where
  arbitrary = genLocals
  shrink = genericShrink

genFunction :: Gen Function
genFunction = Function <$> listOf genLocals <*> genExpression

instance Arbitrary Function where
  arbitrary = genFunction
  shrink = genericShrink

genDataSegment :: Gen DataSegment
genDataSegment = DataSegment <$> genMemoryIndex <*> genExpression <*> genSBS

instance Arbitrary DataSegment where
  arbitrary = genDataSegment
  shrink = genericShrink

genLinkingSubSection :: Gen LinkingSubSection
genLinkingSubSection = LinkingSubSection <$> chooseAny <*> genSBS

instance Arbitrary LinkingSubSection where
  arbitrary = genLinkingSubSection
  shrink = genericShrink

genRelocationType :: Gen RelocationType
genRelocationType =
  Test.QuickCheck.Gen.elements
    [ RWebAssemblyFunctionIndexLEB
    , RWebAssemblyTableIndexSLEB
    , RWebAssemblyTableIndexI32
    , RWebAssemblyMemoryAddrLEB
    , RWebAssemblyMemoryAddrSLEB
    , RWebAssemblyMemoryAddrI32
    , RWebAssemblyTypeIndexLEB
    , RWebAssemblyGlobalIndexLEB
    , RWebAssemblyFunctionOffsetI32
    , RWebAssemblySectionOffsetI32
    ]

instance Arbitrary RelocationType where
  arbitrary = genRelocationType
  shrink = genericShrink

genRelocationEntry :: Gen RelocationEntry
genRelocationEntry = do
  _reloc_type <- genRelocationType
  _reloc_offset <- chooseAny
  _reloc_index <- chooseAny
  _reloc_addend <-
    if _reloc_type `elem`
       [ RWebAssemblyMemoryAddrLEB
       , RWebAssemblyMemoryAddrSLEB
       , RWebAssemblyMemoryAddrI32
       , RWebAssemblyFunctionOffsetI32
       , RWebAssemblySectionOffsetI32
       ]
      then Just <$> chooseAny
      else pure Nothing
  pure $ RelocationEntry _reloc_type _reloc_offset _reloc_index _reloc_addend

instance Arbitrary RelocationEntry where
  arbitrary = genRelocationEntry
  shrink = genericShrink

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
    , DataSection <$> listOf genDataSegment
    ]

instance Arbitrary Section where
  arbitrary = genSection
  shrink = genericShrink

genModule :: Gen Module
genModule = Module <$> listOf genSection
