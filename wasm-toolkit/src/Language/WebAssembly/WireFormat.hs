{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Language.WebAssembly.WireFormat
  ( Name(..)
  , ValueType(..)
  , FunctionType(..)
  , Limits(..)
  , MemoryType(..)
  , ElementType(..)
  , TableType(..)
  , Mutability(..)
  , GlobalType(..)
  , MemoryArgument(..)
  , Instruction(..)
  , Expression(..)
  , Custom(..)
  , FunctionTypeIndex(..)
  , FunctionIndex(..)
  , TableIndex(..)
  , MemoryIndex(..)
  , GlobalIndex(..)
  , LocalIndex(..)
  , LabelIndex(..)
  , ImportDescription(..)
  , Import(..)
  , Table(..)
  , Memory(..)
  , Global(..)
  , ExportDescription(..)
  , Export(..)
  , Start(..)
  , Element(..)
  , Locals(..)
  , Function(..)
  , Data(..)
  , Section(..)
  , getSection
  , putSection
  ) where

import Control.Applicative hiding (Const)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.Foldable
import Data.Word
import Prelude hiding (fail)

newtype Name =
  Name SBS.ShortByteString
  deriving (Eq, Show)

getName :: Get Name
getName = coerce getVecSBS

putName :: Name -> Put
putName = coerce putVecSBS

data ValueType
  = I32
  | I64
  | F32
  | F64
  deriving (Eq, Show)

getValueType :: Get ValueType
getValueType = do
  b <- getWord8
  case b of
    0x7F -> pure I32
    0x7E -> pure I64
    0x7D -> pure F32
    0x7C -> pure F64
    _ -> fail "Language.WebAssembly.WireFormat.getValueType"

putValueType :: ValueType -> Put
putValueType vt =
  putWord8 $
  case vt of
    I32 -> 0x7F
    I64 -> 0x7E
    F32 -> 0x7D
    F64 -> 0x7C

getResultType :: Get [ValueType]
getResultType =
  (do b <- getWord8
      case b of
        0x40 -> pure []
        _ -> fail "Language.WebAssembly.WireFormat.getResultType") <|>
  (do vt <- getValueType
      pure [vt])

putResultType :: [ValueType] -> Put
putResultType resultValueTypes =
  case resultValueTypes of
    [] -> putWord8 0x40
    [t] -> putValueType t
    _ -> error "Language.WebAssembly.WireFormat.putResultType"

data FunctionType = FunctionType
  { parameterTypes, resultTypes :: [ValueType]
  } deriving (Eq, Show)

getFunctionType :: Get FunctionType
getFunctionType = do
  b <- getWord8
  case b of
    0x60 -> FunctionType <$> getVec getValueType <*> getVec getValueType
    _ -> fail "Language.WebAssembly.WireFormat.getFunctionType"

putFunctionType :: FunctionType -> Put
putFunctionType FunctionType {..} = do
  putWord8 0x60
  putVec putValueType parameterTypes
  putVec putValueType resultTypes

data Limits = Limits
  { minLimit :: Word32
  , maxLimit :: Maybe Word32
  } deriving (Eq, Show)

getLimits :: Get Limits
getLimits = do
  b <- getWord8
  case b of
    0x01 -> Limits <$> getU32 <*> fmap Just getU32
    0x00 -> Limits <$> getU32 <*> pure Nothing
    _ -> fail "Language.WebAssembly.WireFormat.getLimits"

putLimits :: Limits -> Put
putLimits Limits {..} =
  case maxLimit of
    Just _max_limit -> do
      putWord8 0x01
      putU32 minLimit
      putU32 _max_limit
    _ -> do
      putWord8 0x00
      putU32 minLimit

newtype MemoryType = MemoryType
  { memoryLimits :: Limits
  } deriving (Eq, Show)

getMemoryType :: Get MemoryType
getMemoryType = coerce getLimits

putMemoryType :: MemoryType -> Put
putMemoryType = coerce putLimits

data ElementType =
  AnyFunc
  deriving (Eq, Show)

getElementType :: Get ElementType
getElementType = do
  b <- getWord8
  case b of
    0x70 -> pure AnyFunc
    _ -> fail "Language.WebAssembly.WireFormat.getElementType"

putElementType :: ElementType -> Put
putElementType et =
  putWord8 $
  case et of
    AnyFunc -> 0x70

data TableType = TableType
  { elementType :: ElementType
  , tableLimits :: Limits
  } deriving (Eq, Show)

getTableType :: Get TableType
getTableType = TableType <$> getElementType <*> getLimits

putTableType :: TableType -> Put
putTableType TableType {..} = do
  putElementType elementType
  putLimits tableLimits

data Mutability
  = Const
  | Var
  deriving (Eq, Show)

getMutability :: Get Mutability
getMutability = do
  b <- getWord8
  case b of
    0x00 -> pure Const
    0x01 -> pure Var
    _ -> fail "Language.WebAssembly.WireFormat.getMutability"

putMutability :: Mutability -> Put
putMutability m =
  putWord8 $
  case m of
    Const -> 0x00
    Var -> 0x01

data GlobalType = GlobalType
  { globalValueType :: ValueType
  , globalMutability :: Mutability
  } deriving (Eq, Show)

getGlobalType :: Get GlobalType
getGlobalType = GlobalType <$> getValueType <*> getMutability

putGlobalType :: GlobalType -> Put
putGlobalType GlobalType {..} = do
  putValueType globalValueType
  putMutability globalMutability

data MemoryArgument = MemoryArgument
  { memoryArgumentAlignment, memoryArgumentOffset :: Word32
  } deriving (Eq, Show)

getMemoryArgument :: Get MemoryArgument
getMemoryArgument = MemoryArgument <$> getU32 <*> getU32

putMemoryArgument :: MemoryArgument -> Put
putMemoryArgument MemoryArgument {..} = do
  putU32 memoryArgumentAlignment
  putU32 memoryArgumentOffset

data Instruction
  = Unreachable
  | Nop
  | Block { blockResultType :: [ValueType]
          , blockInstructions :: [Instruction] }
  | Loop { loopResultType :: [ValueType]
         , loopInstructions :: [Instruction] }
  | If { ifResultType :: [ValueType]
       , thenInstructions :: [Instruction]
       , elseInstructions :: Maybe [Instruction] }
  | Branch { branchLabel :: LabelIndex }
  | BranchIf { branchIfLabel :: LabelIndex }
  | BranchTable { branchTableLabels :: [LabelIndex]
                , branchTableFallbackLabel :: LabelIndex }
  | Return
  | Call { callFunctionIndex :: FunctionIndex }
  | CallIndirect { callIndirectFuctionTypeIndex :: FunctionTypeIndex }
  | Drop
  | Select
  | GetLocal { getLocalIndex :: LocalIndex }
  | SetLocal { setLocalIndex :: LocalIndex }
  | TeeLocal { teeLocalIndex :: LocalIndex }
  | GetGlobal { getGlobalIndex :: GlobalIndex }
  | SetGlobal { setGlobalIndex :: GlobalIndex }
  | I32Load { i32LoadMemoryArgument :: MemoryArgument }
  | I64Load { i64LoadMemoryArgument :: MemoryArgument }
  | F32Load { f32LoadMemoryArgument :: MemoryArgument }
  | F64Load { f64LoadMemoryArgument :: MemoryArgument }
  | I32Load8Signed { i32Load8SignedMemoryArgument :: MemoryArgument }
  | I32Load8Unsigned { i32Load8UnsignedMemoryArgument :: MemoryArgument }
  | I32Load16Signed { i32Load16SignedMemoryArument :: MemoryArgument }
  | I32Load16Unsigned { i32Load16UnsignedMemoryArgument :: MemoryArgument }
  | I64Load8Signed { i64Load8SignedMemoryArgument :: MemoryArgument }
  | I64Load8Unsigned { i64Load8UnsignedMemoryArgument :: MemoryArgument }
  | I64Load16Signed { i64Load16SignedMemoryArgument :: MemoryArgument }
  | I64Load16Unsigned { i64Load16UnsignedMemoryArgument :: MemoryArgument }
  | I64Load32Signed { i64Load32SignedMemoryArgument :: MemoryArgument }
  | I64Load32Unsigned { i64Load32UnsignedMemoryArgument :: MemoryArgument }
  | I32Store { i32StoreMemoryArgument :: MemoryArgument }
  | I64Store { i64StoreMemoryArgument :: MemoryArgument }
  | F32Store { f32StoreMemoryArgument :: MemoryArgument }
  | F64Store { f64StoreMemoryArgument :: MemoryArgument }
  | I32Store8 { i32Store8MemoryArgument :: MemoryArgument }
  | I32Store16 { i32Store16MemoryArgument :: MemoryArgument }
  | I64Store8 { i64Store8MemoryArgument :: MemoryArgument }
  | I64Store16 { i64Store16MemoryArgument :: MemoryArgument }
  | I64Store32 { i64Store32MemoryArgument :: MemoryArgument }
  | MemorySize
  | MemoryGrow
  | I32Eqz
  | I32Eq
  | I32Ne
  | I32LtS
  | I32LtU
  | I32GtS
  | I32GtU
  | I32LeS
  | I32LeU
  | I32GeS
  | I32GeU
  | I64Eqz
  | I64Eq
  | I64Ne
  | I64LtS
  | I64LtU
  | I64GtS
  | I64GtU
  | I64LeS
  | I64LeU
  | I64GeS
  | I64GeU
  | F32Eq
  | F32Ne
  | F32Lt
  | F32Gt
  | F32Le
  | F32Ge
  | F64Eq
  | F64Ne
  | F64Lt
  | F64Gt
  | F64Le
  | F64Ge
  | I32Clz
  | I32Ctz
  | I32Popcnt
  | I32Add
  | I32Sub
  | I32Mul
  | I32DivS
  | I32DivU
  | I32RemS
  | I32RemU
  | I32And
  | I32Or
  | I32Xor
  | I32Shl
  | I32ShrS
  | I32ShrU
  | I32RotL
  | I32RotR
  | I64Clz
  | I64Ctz
  | I64Popcnt
  | I64Add
  | I64Sub
  | I64Mul
  | I64DivS
  | I64DivU
  | I64RemS
  | I64RemU
  | I64And
  | I64Or
  | I64Xor
  | I64Shl
  | I64ShrS
  | I64ShrU
  | I64RotL
  | I64RotR
  | F32Abs
  | F32Neg
  | F32Ceil
  | F32Floor
  | F32Trunc
  | F32Nearest
  | F32Sqrt
  | F32Add
  | F32Sub
  | F32Mul
  | F32Div
  | F32Min
  | F32Max
  | F32Copysign
  | F64Abs
  | F64Neg
  | F64Ceil
  | F64Floor
  | F64Trunc
  | F64Nearest
  | F64Sqrt
  | F64Add
  | F64Sub
  | F64Mul
  | F64Div
  | F64Min
  | F64Max
  | F64Copysign
  | I32WrapFromI64
  | I32TruncSFromF32
  | I32TruncUFromF32
  | I32TruncSFromF64
  | I32TruncUFromF64
  | I64ExtendSFromI32
  | I64ExtendUFromI32
  | I64TruncSFromF32
  | I64TruncUFromF32
  | I64TruncSFromF64
  | I64TruncUFromF64
  | F32ConvertSFromI32
  | F32ConvertUFromI32
  | F32ConvertSFromI64
  | F32ConvertUFromI64
  | F32DemoteFromF64
  | F64ConvertSFromI32
  | F64ConvertUFromI32
  | F64ConvertSFromI64
  | F64ConvertUFromI64
  | F64PromoteFromF32
  | I32ReinterpretFromF32
  | I64ReinterpretFromF64
  | F32ReinterpretFromI32
  | F64ReinterpretFromI64
  deriving (Eq, Show)

getInstruction :: Get Instruction
getInstruction = do
  b <- getWord8
  case b of
    0x00 -> pure Unreachable
    0x01 -> pure Nop
    0x02 ->
      Block <$> getResultType <*> getMany getInstruction <* expectWord8 0x0B
    0x03 ->
      Loop <$> getResultType <*> getMany getInstruction <* expectWord8 0x0B
    0x04 ->
      If <$> getResultType <*> getMany getInstruction <*>
      ((expectWord8 0x05 *> (Just <$> getMany getInstruction)) <|> pure Nothing) <*
      expectWord8 0x0B
    0x0C -> Branch <$> getLabelIndex
    0x0D -> BranchIf <$> getLabelIndex
    0x0E -> BranchTable <$> getVec getLabelIndex <*> getLabelIndex
    0x0F -> pure Return
    0x10 -> Call <$> getFunctionIndex
    0x11 -> CallIndirect <$> getFunctionTypeIndex
    0x1A -> pure Drop
    0x1B -> pure Select
    0x20 -> GetLocal <$> getLocalIndex'
    0x21 -> SetLocal <$> getLocalIndex'
    0x22 -> TeeLocal <$> getLocalIndex'
    0x23 -> GetGlobal <$> getGlobalIndex'
    0x24 -> SetGlobal <$> getGlobalIndex'
    0x28 -> I32Load <$> getMemoryArgument
    0x29 -> I64Load <$> getMemoryArgument
    0x2A -> F32Load <$> getMemoryArgument
    0x2B -> F64Load <$> getMemoryArgument
    0x2C -> I32Load8Signed <$> getMemoryArgument
    0x2D -> I32Load8Unsigned <$> getMemoryArgument
    0x2E -> I32Load16Signed <$> getMemoryArgument
    0x2F -> I32Load16Unsigned <$> getMemoryArgument
    0x30 -> I64Load8Signed <$> getMemoryArgument
    0x31 -> I64Load8Unsigned <$> getMemoryArgument
    0x32 -> I64Load16Signed <$> getMemoryArgument
    0x33 -> I64Load16Unsigned <$> getMemoryArgument
    0x34 -> I64Load32Signed <$> getMemoryArgument
    0x35 -> I64Load32Unsigned <$> getMemoryArgument
    0x36 -> I32Store <$> getMemoryArgument
    0x37 -> I64Store <$> getMemoryArgument
    0x38 -> F32Store <$> getMemoryArgument
    0x39 -> F64Store <$> getMemoryArgument
    0x3A -> I32Store8 <$> getMemoryArgument
    0x3B -> I32Store16 <$> getMemoryArgument
    0x3C -> I64Store8 <$> getMemoryArgument
    0x3D -> I64Store16 <$> getMemoryArgument
    0x3E -> I64Store32 <$> getMemoryArgument
    0x3F -> MemorySize <$ expectWord8 0x00
    0x40 -> MemoryGrow <$ expectWord8 0x00
    0x45 -> pure I32Eqz
    0x46 -> pure I32Eq
    0x47 -> pure I32Ne
    0x48 -> pure I32LtS
    0x49 -> pure I32LtU
    0x4A -> pure I32GtS
    0x4B -> pure I32GtU
    0x4C -> pure I32LeS
    0x4D -> pure I32LeU
    0x4E -> pure I32GeS
    0x4F -> pure I32GeU
    0x50 -> pure I64Eqz
    0x51 -> pure I64Eq
    0x52 -> pure I64Ne
    0x53 -> pure I64LtS
    0x54 -> pure I64LtU
    0x55 -> pure I64GtS
    0x56 -> pure I64GtU
    0x57 -> pure I64LeS
    0x58 -> pure I64LeU
    0x59 -> pure I64GeS
    0x5A -> pure I64GeU
    0x5B -> pure F32Eq
    0x5C -> pure F32Ne
    0x5D -> pure F32Lt
    0x5E -> pure F32Gt
    0x5F -> pure F32Le
    0x60 -> pure F32Ge
    0x61 -> pure F64Eq
    0x62 -> pure F64Ne
    0x63 -> pure F64Lt
    0x64 -> pure F64Gt
    0x65 -> pure F64Le
    0x66 -> pure F64Ge
    0x67 -> pure I32Clz
    0x68 -> pure I32Ctz
    0x69 -> pure I32Popcnt
    0x6A -> pure I32Add
    0x6B -> pure I32Sub
    0x6C -> pure I32Mul
    0x6D -> pure I32DivS
    0x6E -> pure I32DivU
    0x6F -> pure I32RemS
    0x70 -> pure I32RemU
    0x71 -> pure I32And
    0x72 -> pure I32Or
    0x73 -> pure I32Xor
    0x74 -> pure I32Shl
    0x75 -> pure I32ShrS
    0x76 -> pure I32ShrU
    0x77 -> pure I32RotL
    0x78 -> pure I32RotR
    0x79 -> pure I64Clz
    0x7A -> pure I64Ctz
    0x7B -> pure I64Popcnt
    0x7C -> pure I64Add
    0x7D -> pure I64Sub
    0x7E -> pure I64Mul
    0x7F -> pure I64DivS
    0x80 -> pure I64DivU
    0x81 -> pure I64RemS
    0x82 -> pure I64RemU
    0x83 -> pure I64And
    0x84 -> pure I64Or
    0x85 -> pure I64Xor
    0x86 -> pure I64Shl
    0x87 -> pure I64ShrS
    0x88 -> pure I64ShrU
    0x89 -> pure I64RotL
    0x8A -> pure I64RotR
    0x8B -> pure F32Abs
    0x8C -> pure F32Neg
    0x8D -> pure F32Ceil
    0x8E -> pure F32Floor
    0x8F -> pure F32Trunc
    0x90 -> pure F32Nearest
    0x91 -> pure F32Sqrt
    0x92 -> pure F32Add
    0x93 -> pure F32Sub
    0x94 -> pure F32Mul
    0x95 -> pure F32Div
    0x96 -> pure F32Min
    0x97 -> pure F32Max
    0x98 -> pure F32Copysign
    0x99 -> pure F64Abs
    0x9A -> pure F64Neg
    0x9B -> pure F64Ceil
    0x9C -> pure F64Floor
    0x9D -> pure F64Trunc
    0x9E -> pure F64Nearest
    0x9F -> pure F64Sqrt
    0xA0 -> pure F64Add
    0xA1 -> pure F64Sub
    0xA2 -> pure F64Mul
    0xA3 -> pure F64Div
    0xA4 -> pure F64Min
    0xA5 -> pure F64Max
    0xA6 -> pure F64Copysign
    0xA7 -> pure I32WrapFromI64
    0xA8 -> pure I32TruncSFromF32
    0xA9 -> pure I32TruncUFromF32
    0xAA -> pure I32TruncSFromF64
    0xAB -> pure I32TruncUFromF64
    0xAC -> pure I64ExtendSFromI32
    0xAD -> pure I64ExtendUFromI32
    0xAE -> pure I64TruncSFromF32
    0xAF -> pure I64TruncUFromF32
    0xB0 -> pure I64TruncSFromF64
    0xB1 -> pure I64TruncUFromF64
    0xB2 -> pure F32ConvertSFromI32
    0xB3 -> pure F32ConvertUFromI32
    0xB4 -> pure F32ConvertSFromI64
    0xB5 -> pure F32ConvertUFromI64
    0xB6 -> pure F32DemoteFromF64
    0xB7 -> pure F64ConvertSFromI32
    0xB8 -> pure F64ConvertUFromI32
    0xB9 -> pure F64ConvertSFromI64
    0xBA -> pure F64ConvertUFromI64
    0xBB -> pure F64PromoteFromF32
    0xBC -> pure I32ReinterpretFromF32
    0xBD -> pure I64ReinterpretFromF64
    0xBE -> pure F32ReinterpretFromI32
    0xBF -> pure F64ReinterpretFromI64
    _ -> fail "Language.WebAssembly.WireFormat.getInstruction"

putInstruction :: Instruction -> Put
putInstruction instr =
  case instr of
    Unreachable -> putWord8 0x00
    Nop -> putWord8 0x01
    Block {..} -> do
      putWord8 0x02
      putResultType blockResultType
      putMany putInstruction blockInstructions
      putWord8 0x0B
    Loop {..} -> do
      putWord8 0x03
      putResultType loopResultType
      putMany putInstruction loopInstructions
      putWord8 0x0B
    If {..} -> do
      putWord8 0x04
      putResultType ifResultType
      putMany putInstruction thenInstructions
      case elseInstructions of
        Just _else_instrs -> do
          putWord8 0x05
          putMany putInstruction _else_instrs
        _ -> pure ()
      putWord8 0x0B
    Branch {..} -> do
      putWord8 0x0C
      putLabelIndex branchLabel
    BranchIf {..} -> do
      putWord8 0x0D
      putLabelIndex branchIfLabel
    BranchTable {..} -> do
      putWord8 0x0E
      putVec putLabelIndex branchTableLabels
      putLabelIndex branchTableFallbackLabel
    Return -> putWord8 0x0F
    Call {..} -> do
      putWord8 0x10
      putFunctionIndex callFunctionIndex
    CallIndirect {..} -> do
      putWord8 0x11
      putFunctionTypeIndex callIndirectFuctionTypeIndex
    Drop -> putWord8 0x1A
    Select -> putWord8 0x1B
    GetLocal {..} -> do
      putWord8 0x20
      putLocalIndex getLocalIndex
    SetLocal {..} -> do
      putWord8 0x21
      putLocalIndex setLocalIndex
    TeeLocal {..} -> do
      putWord8 0x22
      putLocalIndex teeLocalIndex
    GetGlobal {..} -> do
      putWord8 0x23
      putGlobalIndex getGlobalIndex
    SetGlobal {..} -> do
      putWord8 0x24
      putGlobalIndex setGlobalIndex
    I32Load {..} -> do
      putWord8 0x28
      putMemoryArgument i32LoadMemoryArgument
    I64Load {..} -> do
      putWord8 0x29
      putMemoryArgument i64LoadMemoryArgument
    F32Load {..} -> do
      putWord8 0x2A
      putMemoryArgument f32LoadMemoryArgument
    F64Load {..} -> do
      putWord8 0x2B
      putMemoryArgument f64LoadMemoryArgument
    I32Load8Signed {..} -> do
      putWord8 0x2C
      putMemoryArgument i32Load8SignedMemoryArgument
    I32Load8Unsigned {..} -> do
      putWord8 0x2D
      putMemoryArgument i32Load8UnsignedMemoryArgument
    I32Load16Signed {..} -> do
      putWord8 0x2E
      putMemoryArgument i32Load16SignedMemoryArument
    I32Load16Unsigned {..} -> do
      putWord8 0x2F
      putMemoryArgument i32Load16UnsignedMemoryArgument
    I64Load8Signed {..} -> do
      putWord8 0x30
      putMemoryArgument i64Load8SignedMemoryArgument
    I64Load8Unsigned {..} -> do
      putWord8 0x31
      putMemoryArgument i64Load8UnsignedMemoryArgument
    I64Load16Signed {..} -> do
      putWord8 0x32
      putMemoryArgument i64Load16SignedMemoryArgument
    I64Load16Unsigned {..} -> do
      putWord8 0x33
      putMemoryArgument i64Load16UnsignedMemoryArgument
    I64Load32Signed {..} -> do
      putWord8 0x34
      putMemoryArgument i64Load32SignedMemoryArgument
    I64Load32Unsigned {..} -> do
      putWord8 0x35
      putMemoryArgument i64Load32UnsignedMemoryArgument
    I32Store {..} -> do
      putWord8 0x36
      putMemoryArgument i32StoreMemoryArgument
    I64Store {..} -> do
      putWord8 0x37
      putMemoryArgument i64StoreMemoryArgument
    F32Store {..} -> do
      putWord8 0x38
      putMemoryArgument f32StoreMemoryArgument
    F64Store {..} -> do
      putWord8 0x39
      putMemoryArgument f64StoreMemoryArgument
    I32Store8 {..} -> do
      putWord8 0x3A
      putMemoryArgument i32Store8MemoryArgument
    I32Store16 {..} -> do
      putWord8 0x3B
      putMemoryArgument i32Store16MemoryArgument
    I64Store8 {..} -> do
      putWord8 0x3C
      putMemoryArgument i64Store8MemoryArgument
    I64Store16 {..} -> do
      putWord8 0x3D
      putMemoryArgument i64Store16MemoryArgument
    I64Store32 {..} -> do
      putWord8 0x3E
      putMemoryArgument i64Store32MemoryArgument
    MemorySize -> do
      putWord8 0x3F
      putWord8 0x00
    MemoryGrow -> do
      putWord8 0x40
      putWord8 0x00
    I32Eqz -> putWord8 0x45
    I32Eq -> putWord8 0x46
    I32Ne -> putWord8 0x47
    I32LtS -> putWord8 0x48
    I32LtU -> putWord8 0x49
    I32GtS -> putWord8 0x4A
    I32GtU -> putWord8 0x4B
    I32LeS -> putWord8 0x4C
    I32LeU -> putWord8 0x4D
    I32GeS -> putWord8 0x4E
    I32GeU -> putWord8 0x4F
    I64Eqz -> putWord8 0x50
    I64Eq -> putWord8 0x51
    I64Ne -> putWord8 0x52
    I64LtS -> putWord8 0x53
    I64LtU -> putWord8 0x54
    I64GtS -> putWord8 0x55
    I64GtU -> putWord8 0x56
    I64LeS -> putWord8 0x57
    I64LeU -> putWord8 0x58
    I64GeS -> putWord8 0x59
    I64GeU -> putWord8 0x5A
    F32Eq -> putWord8 0x5B
    F32Ne -> putWord8 0x5C
    F32Lt -> putWord8 0x5D
    F32Gt -> putWord8 0x5E
    F32Le -> putWord8 0x5F
    F32Ge -> putWord8 0x60
    F64Eq -> putWord8 0x61
    F64Ne -> putWord8 0x62
    F64Lt -> putWord8 0x63
    F64Gt -> putWord8 0x64
    F64Le -> putWord8 0x65
    F64Ge -> putWord8 0x66
    I32Clz -> putWord8 0x67
    I32Ctz -> putWord8 0x68
    I32Popcnt -> putWord8 0x69
    I32Add -> putWord8 0x6A
    I32Sub -> putWord8 0x6B
    I32Mul -> putWord8 0x6C
    I32DivS -> putWord8 0x6D
    I32DivU -> putWord8 0x6E
    I32RemS -> putWord8 0x6F
    I32RemU -> putWord8 0x70
    I32And -> putWord8 0x71
    I32Or -> putWord8 0x72
    I32Xor -> putWord8 0x73
    I32Shl -> putWord8 0x74
    I32ShrS -> putWord8 0x75
    I32ShrU -> putWord8 0x76
    I32RotL -> putWord8 0x77
    I32RotR -> putWord8 0x78
    I64Clz -> putWord8 0x79
    I64Ctz -> putWord8 0x7A
    I64Popcnt -> putWord8 0x7B
    I64Add -> putWord8 0x7C
    I64Sub -> putWord8 0x7D
    I64Mul -> putWord8 0x7E
    I64DivS -> putWord8 0x7F
    I64DivU -> putWord8 0x80
    I64RemS -> putWord8 0x81
    I64RemU -> putWord8 0x82
    I64And -> putWord8 0x83
    I64Or -> putWord8 0x84
    I64Xor -> putWord8 0x85
    I64Shl -> putWord8 0x86
    I64ShrS -> putWord8 0x87
    I64ShrU -> putWord8 0x88
    I64RotL -> putWord8 0x89
    I64RotR -> putWord8 0x8A
    F32Abs -> putWord8 0x8B
    F32Neg -> putWord8 0x8C
    F32Ceil -> putWord8 0x8D
    F32Floor -> putWord8 0x8E
    F32Trunc -> putWord8 0x8F
    F32Nearest -> putWord8 0x90
    F32Sqrt -> putWord8 0x91
    F32Add -> putWord8 0x92
    F32Sub -> putWord8 0x93
    F32Mul -> putWord8 0x94
    F32Div -> putWord8 0x95
    F32Min -> putWord8 0x96
    F32Max -> putWord8 0x97
    F32Copysign -> putWord8 0x98
    F64Abs -> putWord8 0x99
    F64Neg -> putWord8 0x9A
    F64Ceil -> putWord8 0x9B
    F64Floor -> putWord8 0x9C
    F64Trunc -> putWord8 0x9D
    F64Nearest -> putWord8 0x9E
    F64Sqrt -> putWord8 0x9F
    F64Add -> putWord8 0xA0
    F64Sub -> putWord8 0xA1
    F64Mul -> putWord8 0xA2
    F64Div -> putWord8 0xA3
    F64Min -> putWord8 0xA4
    F64Max -> putWord8 0xA5
    F64Copysign -> putWord8 0xA6
    I32WrapFromI64 -> putWord8 0xA7
    I32TruncSFromF32 -> putWord8 0xA8
    I32TruncUFromF32 -> putWord8 0xA9
    I32TruncSFromF64 -> putWord8 0xAA
    I32TruncUFromF64 -> putWord8 0xAB
    I64ExtendSFromI32 -> putWord8 0xAC
    I64ExtendUFromI32 -> putWord8 0xAD
    I64TruncSFromF32 -> putWord8 0xAE
    I64TruncUFromF32 -> putWord8 0xAF
    I64TruncSFromF64 -> putWord8 0xB0
    I64TruncUFromF64 -> putWord8 0xB1
    F32ConvertSFromI32 -> putWord8 0xB2
    F32ConvertUFromI32 -> putWord8 0xB3
    F32ConvertSFromI64 -> putWord8 0xB4
    F32ConvertUFromI64 -> putWord8 0xB5
    F32DemoteFromF64 -> putWord8 0xB6
    F64ConvertSFromI32 -> putWord8 0xB7
    F64ConvertUFromI32 -> putWord8 0xB8
    F64ConvertSFromI64 -> putWord8 0xB9
    F64ConvertUFromI64 -> putWord8 0xBA
    F64PromoteFromF32 -> putWord8 0xBB
    I32ReinterpretFromF32 -> putWord8 0xBC
    I64ReinterpretFromF64 -> putWord8 0xBD
    F32ReinterpretFromI32 -> putWord8 0xBE
    F64ReinterpretFromI64 -> putWord8 0xBF

newtype Expression = Expression
  { instructions :: [Instruction]
  } deriving (Eq, Show)

getExpression :: Get Expression
getExpression = coerce (getMany getInstruction) <* expectWord8 0x0B

putExpression :: Expression -> Put
putExpression expr = do
  putMany putInstruction $ coerce expr
  putWord8 0x0B

data Custom = Custom
  { customName :: Name
  , customContent :: SBS.ShortByteString
  } deriving (Eq, Show)

getCustom :: Word32 -> Get Custom
getCustom l = do
  o0 <- bytesRead
  n <- getName
  o1 <- bytesRead
  buf <- getSBS $ l - fromIntegral (o1 - o0)
  pure $ Custom n buf

putCustom :: Custom -> Put
putCustom Custom {..} = do
  putName customName
  putSBS customContent

newtype FunctionTypeIndex =
  FunctionTypeIndex Word32
  deriving (Eq, Show)

getFunctionTypeIndex :: Get FunctionTypeIndex
getFunctionTypeIndex = coerce getU32

putFunctionTypeIndex :: FunctionTypeIndex -> Put
putFunctionTypeIndex = coerce putU32

newtype FunctionIndex =
  FunctionIndex Word32
  deriving (Eq, Show)

getFunctionIndex :: Get FunctionIndex
getFunctionIndex = coerce getU32

putFunctionIndex :: FunctionIndex -> Put
putFunctionIndex = coerce putU32

newtype TableIndex =
  TableIndex Word32
  deriving (Eq, Show)

getTableIndex :: Get TableIndex
getTableIndex = coerce getU32

putTableIndex :: TableIndex -> Put
putTableIndex = coerce putU32

newtype MemoryIndex =
  MemoryIndex Word32
  deriving (Eq, Show)

getMemoryIndex :: Get MemoryIndex
getMemoryIndex = coerce getU32

putMemoryIndex :: MemoryIndex -> Put
putMemoryIndex = coerce putU32

newtype GlobalIndex =
  GlobalIndex Word32
  deriving (Eq, Show)

getGlobalIndex' :: Get GlobalIndex
getGlobalIndex' = coerce getU32

putGlobalIndex :: GlobalIndex -> Put
putGlobalIndex = coerce putU32

newtype LocalIndex =
  LocalIndex Word32
  deriving (Eq, Show)

getLocalIndex' :: Get LocalIndex
getLocalIndex' = coerce getU32

putLocalIndex :: LocalIndex -> Put
putLocalIndex = coerce putU32

newtype LabelIndex =
  LabelIndex Word32
  deriving (Eq, Show)

getLabelIndex :: Get LabelIndex
getLabelIndex = coerce getU32

putLabelIndex :: LabelIndex -> Put
putLabelIndex = coerce putU32

data ImportDescription
  = ImportFunction FunctionTypeIndex
  | ImportTable TableType
  | ImportMemory MemoryType
  | ImportGlobal GlobalType
  deriving (Eq, Show)

getImportDescription :: Get ImportDescription
getImportDescription = do
  b <- getWord8
  case b of
    0x00 -> ImportFunction <$> getFunctionTypeIndex
    0x01 -> ImportTable <$> getTableType
    0x02 -> ImportMemory <$> getMemoryType
    0x03 -> ImportGlobal <$> getGlobalType
    _ -> fail "Language.WebAssembly.WireFormat.getImportDescription"

putImportDescription :: ImportDescription -> Put
putImportDescription desc =
  case desc of
    ImportFunction x -> do
      putWord8 0x00
      putFunctionTypeIndex x
    ImportTable tt -> do
      putWord8 0x01
      putTableType tt
    ImportMemory mt -> do
      putWord8 0x02
      putMemoryType mt
    ImportGlobal gt -> do
      putWord8 0x03
      putGlobalType gt

data Import = Import
  { moduleName, importName :: Name
  , importDescription :: ImportDescription
  } deriving (Eq, Show)

getImport :: Get Import
getImport = Import <$> getName <*> getName <*> getImportDescription

putImport :: Import -> Put
putImport Import {..} = do
  putName moduleName
  putName importName
  putImportDescription importDescription

newtype Table = Table
  { tableType :: TableType
  } deriving (Eq, Show)

getTable :: Get Table
getTable = coerce getTableType

putTable :: Table -> Put
putTable = coerce putTableType

newtype Memory = Memory
  { memoryType :: MemoryType
  } deriving (Eq, Show)

getMemory :: Get Memory
getMemory = coerce getMemoryType

putMemory :: Memory -> Put
putMemory = coerce putMemoryType

data Global = Global
  { globalType :: GlobalType
  , globalInitialValue :: Expression
  } deriving (Eq, Show)

getGlobal :: Get Global
getGlobal = Global <$> getGlobalType <*> getExpression

putGlobal :: Global -> Put
putGlobal Global {..} = do
  putGlobalType globalType
  putExpression globalInitialValue

data ExportDescription
  = ExportFunction FunctionIndex
  | ExportTable TableIndex
  | ExportMemory MemoryIndex
  | ExportGlobal GlobalIndex
  deriving (Eq, Show)

getExportDescription :: Get ExportDescription
getExportDescription = do
  b <- getWord8
  case b of
    0x00 -> ExportFunction <$> getFunctionIndex
    0x01 -> ExportTable <$> getTableIndex
    0x02 -> ExportMemory <$> getMemoryIndex
    0x03 -> ExportGlobal <$> getGlobalIndex'
    _ -> fail "Language.WebAssembly.WireFormat.getExportDescription"

putExportDescription :: ExportDescription -> Put
putExportDescription d =
  case d of
    ExportFunction x -> do
      putWord8 0x00
      putFunctionIndex x
    ExportTable x -> do
      putWord8 0x01
      putTableIndex x
    ExportMemory x -> do
      putWord8 0x02
      putMemoryIndex x
    ExportGlobal x -> do
      putWord8 0x03
      putGlobalIndex x

data Export = Export
  { exportName :: Name
  , exportDescription :: ExportDescription
  } deriving (Eq, Show)

getExport :: Get Export
getExport = Export <$> getName <*> getExportDescription

putExport :: Export -> Put
putExport Export {..} = do
  putName exportName
  putExportDescription exportDescription

newtype Start = Start
  { startFunctionIndex :: FunctionIndex
  } deriving (Eq, Show)

getStart :: Get Start
getStart = coerce getFunctionIndex

putStart :: Start -> Put
putStart = coerce putFunctionIndex

data Element = Element
  { tableIndex :: TableIndex
  , tableOffset :: Expression
  , tableInitialValues :: [FunctionIndex]
  } deriving (Eq, Show)

getElement :: Get Element
getElement =
  Element <$> getTableIndex <*> getExpression <*> getVec getFunctionIndex

putElement :: Element -> Put
putElement Element {..} = do
  putTableIndex tableIndex
  putExpression tableOffset
  putVec putFunctionIndex tableInitialValues

data Locals = Locals
  { localsCount :: Word32
  , localsType :: ValueType
  } deriving (Eq, Show)

getLocals :: Get Locals
getLocals = Locals <$> getU32 <*> getValueType

putLocals :: Locals -> Put
putLocals Locals {..} = do
  putU32 localsCount
  putValueType localsType

data Function = Function
  { functionLocals :: [Locals]
  , functionBody :: Expression
  } deriving (Eq, Show)

getFunction :: Get Function
getFunction = Function <$> getVec getLocals <*> getExpression

putFunction :: Function -> Put
putFunction Function {..} = do
  putVec putLocals functionLocals
  putExpression functionBody

data Data = Data
  { memoryIndex :: MemoryIndex
  , memoryOffset :: Expression
  , memoryInitialBytes :: SBS.ShortByteString
  } deriving (Eq, Show)

getData :: Get Data
getData = Data <$> getMemoryIndex <*> getExpression <*> getVecSBS

putData :: Data -> Put
putData Data {..} = do
  putMemoryIndex memoryIndex
  putExpression memoryOffset
  putVecSBS memoryInitialBytes

data Section
  = CustomSection { custom :: Custom }
  | TypeSection { types :: [FunctionType] }
  | ImportSection { imports :: [Import] }
  | FunctionSection { functionTypeIndices :: [FunctionTypeIndex] }
  | TableSection { tables :: [Table] }
  | MemorySection { memories :: [Memory] }
  | GlobalSection { globals :: [Global] }
  | ExportSection { exports :: [Export] }
  | StartSection { start :: Start }
  | ElementSection { elements :: [Element] }
  | CodeSection { functions' :: [Function] }
  | DataSection { dataSegments :: [Data] }
  deriving (Eq, Show)

getSection :: Get Section
getSection = do
  b <- getWord8
  case b of
    0 -> do
      l <- getU32
      CustomSection <$> getCustom l
    1 -> getU32 *> (TypeSection <$> getVec getFunctionType)
    2 -> getU32 *> (ImportSection <$> getVec getImport)
    3 -> getU32 *> (FunctionSection <$> getVec getFunctionTypeIndex)
    4 -> getU32 *> (TableSection <$> getVec getTable)
    5 -> getU32 *> (MemorySection <$> getVec getMemory)
    6 -> getU32 *> (GlobalSection <$> getVec getGlobal)
    7 -> getU32 *> (ExportSection <$> getVec getExport)
    8 -> getU32 *> (StartSection <$> getStart)
    9 -> getU32 *> (ElementSection <$> getVec getElement)
    10 -> getU32 *> (CodeSection <$> getMany (getU32 *> getFunction))
    11 -> getU32 *> (DataSection <$> getVec getData)
    _ -> fail "Language.WebAssembly.WireFormat.getSection"

putSection :: Section -> Put
putSection sec =
  case sec of
    CustomSection {..} -> do
      putWord8 0
      putWithLength $ putCustom custom
    TypeSection {..} -> do
      putWord8 1
      putWithLength $ putVec putFunctionType types
    ImportSection {..} -> do
      putWord8 2
      putWithLength $ putVec putImport imports
    FunctionSection {..} -> do
      putWord8 3
      putWithLength $ putVec putFunctionTypeIndex functionTypeIndices
    TableSection {..} -> do
      putWord8 4
      putWithLength $ putVec putTable tables
    MemorySection {..} -> do
      putWord8 5
      putWithLength $ putVec putMemory memories
    GlobalSection {..} -> do
      putWord8 6
      putWithLength $ putVec putGlobal globals
    ExportSection {..} -> do
      putWord8 7
      putWithLength $ putVec putExport exports
    StartSection {..} -> do
      putWord8 8
      putWithLength $ putStart start
    ElementSection {..} -> do
      putWord8 9
      putWithLength $ putVec putElement elements
    CodeSection {..} -> do
      putWord8 10
      putWithLength $ putMany (putWithLength . putFunction) functions'
    DataSection {..} -> do
      putWord8 11
      putWithLength $ putVec putData dataSegments

expectWord8 :: Word8 -> Get ()
expectWord8 x = do
  b <- getWord8
  unless (b == x) $
    fail $
    "Language.WebAssembly.WireFormat.expectWord8: expected " <> show x <>
    ", but got " <>
    show b

getUnsignedLEB ::
     forall a. (Integral a, Bits a)
  => Int
  -> Get a
getUnsignedLEB bits
  | bits <= 0 = fail "Language.WebAssembly.WireFormat.getUnsignedLEB"
  | otherwise = do
    b <- getWord8
    let b_masked = b .&. 0x7F
    if b == b_masked
      then pure $ fromIntegral b
      else do
        r <- getUnsignedLEB (bits - 7)
        pure $ (r `shiftL` 7) .|. fromIntegral b_masked

putUnsignedLEB :: (Integral a, Bits a) => Int -> a -> Put
putUnsignedLEB bits a
  | bits <= 0 = error "Language.WebAssembly.WireFormat.putUnsignedLEB"
  | otherwise =
    case (a `shiftR` 7, a .&. 0x7F) of
      (0, r) -> putWord8 $ fromIntegral r
      (x, r) -> do
        putWord8 $ fromIntegral r .|. 0x80
        putUnsignedLEB (bits - 7) x

getU32 :: Get Word32
getU32 = getUnsignedLEB 32

putU32 :: Word32 -> Put
putU32 = putUnsignedLEB 32

getVec :: Get a -> Get [a]
getVec g = do
  n <- getU32
  replicateM (fromIntegral n) g

putVec :: (a -> Put) -> [a] -> Put
putVec p v = do
  putU32 (fromIntegral (length v) :: Word32)
  for_ v p

getMany :: Get a -> Get [a]
getMany = many

putMany :: (a -> Put) -> [a] -> Put
putMany = traverse_

getSBS :: Word32 -> Get SBS.ShortByteString
getSBS n
  | n < 0 = fail "Language.WebAssembly.WireFormat.getSBS"
  | otherwise = SBS.pack <$> replicateM (fromIntegral n) getWord8

putSBS :: SBS.ShortByteString -> Put
putSBS = putMany putWord8 . SBS.unpack

getVecSBS :: Get SBS.ShortByteString
getVecSBS = SBS.pack <$> getVec getWord8

putVecSBS :: SBS.ShortByteString -> Put
putVecSBS s = do
  putU32 $ fromIntegral $ SBS.length s
  putSBS s

putWithLength :: Put -> Put
putWithLength p = do
  let buf = runPut p
  putU32 $ fromIntegral $ LBS.length buf
  putLazyByteString buf
