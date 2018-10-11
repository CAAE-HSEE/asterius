{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.NewMarshal
  ( MarshalError(..)
  , makeModule
  ) where

import Asterius.Internals
import qualified Asterius.Internals.DList as DList
import Asterius.TypeInfer
import Asterius.Types
import Control.Exception
import Control.Monad.Except
import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.List
import qualified Data.Map.Strict as Map
import Data.Traversable
import Data.Word
import qualified Language.WebAssembly.WireFormat as Wasm

data MarshalError
  = DuplicateFunctionImport
  | DuplicateGlobalImport
  | InvalidParameterType
  | InvalidLocalType
  | UnsupportedExpression Expression
  deriving (Show)

instance Exception MarshalError

data ModuleSymbolTable = ModuleSymbolTable
  { functionTypeSymbols :: Map.Map SBS.ShortByteString Wasm.FunctionTypeIndex
  , functionSymbols :: Map.Map SBS.ShortByteString Wasm.FunctionIndex
  }

makeModuleSymbolTable ::
     MonadError MarshalError m => Module -> m ModuleSymbolTable
makeModuleSymbolTable Module {..} = do
  let _has_dup l = length l /= length (nub l)
      _func_import_syms =
        [internalName | FunctionImport {..} <- functionImports]
      _func_syms = Map.keys functionMap'
      _func_conflict_syms = _func_import_syms `intersect` _func_syms
  if _has_dup _func_import_syms
    then throwError DuplicateFunctionImport
    else pure
           ModuleSymbolTable
             { functionTypeSymbols =
                 Map.fromDistinctAscList $
                 zip (Map.keys functionTypeMap) (coerce [0 :: Word32 ..])
             , functionSymbols =
                 Map.fromList $
                 zip (_func_import_syms <> _func_syms) (coerce [0 :: Word32 ..])
             }

makeResultType :: ValueType -> [Wasm.ValueType]
makeResultType vt =
  case vt of
    None -> []
    I32 -> [Wasm.I32]
    I64 -> [Wasm.I64]
    F32 -> [Wasm.F32]
    F64 -> [Wasm.F64]

makeTypeSection :: MonadError MarshalError m => Module -> m Wasm.Section
makeTypeSection Module {..} = do
  _func_types <-
    for (Map.elems functionTypeMap) $ \FunctionType {..} -> do
      _param_types <-
        for paramTypes $ \case
          None -> throwError InvalidParameterType
          I32 -> pure Wasm.I32
          I64 -> pure Wasm.I64
          F32 -> pure Wasm.F32
          F64 -> pure Wasm.F64
      let _result_type = makeResultType returnType
      pure
        Wasm.FunctionType
          {parameterTypes = _param_types, resultTypes = _result_type}
  pure Wasm.TypeSection {types = _func_types}

makeImportSection ::
     MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeImportSection Module {..} ModuleSymbolTable {..} =
  pure
    Wasm.ImportSection
      { imports =
          [ Wasm.Import
            { moduleName = coerce externalModuleName
            , importName = coerce externalBaseName
            , importDescription =
                Wasm.ImportFunction $ functionTypeSymbols ! functionTypeName
            }
          | FunctionImport {..} <- functionImports
          ]
      }

makeFunctionSection ::
     MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeFunctionSection Module {..} ModuleSymbolTable {..} =
  pure
    Wasm.FunctionSection
      { functionTypeIndices =
          [ functionTypeSymbols ! functionTypeName
          | Function {..} <- Map.elems functionMap'
          ]
      }

makeTableSection :: MonadError MarshalError m => Module -> m Wasm.Section
makeTableSection Module {..} =
  pure
    Wasm.TableSection
      { tables =
          [ Wasm.Table
              { tableType =
                  Wasm.TableType
                    { elementType = Wasm.AnyFunc
                    , tableLimits =
                        Wasm.Limits {minLimit = 0, maxLimit = Nothing}
                    }
              }
          ]
      }

makeMemorySection :: MonadError MarshalError m => Module -> m Wasm.Section
makeMemorySection Module {..} =
  pure
    Wasm.MemorySection
      { memories =
          case memory of
            Just Memory {..} ->
              [ Wasm.Memory
                  { memoryType =
                      Wasm.MemoryType
                        { memoryLimits =
                            Wasm.Limits
                              { minLimit = initialPages
                              , maxLimit = Just maximumPages
                              }
                        }
                  }
              ]
            _ -> []
      }

makeExportSection ::
     MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeExportSection Module {..} ModuleSymbolTable {..} =
  pure
    Wasm.ExportSection
      { exports =
          [ Wasm.Export
            { exportName = coerce externalName
            , exportDescription =
                Wasm.ExportFunction $ functionSymbols ! internalName
            }
          | FunctionExport {..} <- functionExports
          ] <>
          case memory of
            Just Memory {..}
              | not $ SBS.null exportName ->
                [ Wasm.Export
                    { exportName = coerce exportName
                    , exportDescription = Wasm.ExportMemory $ Wasm.MemoryIndex 0
                    }
                ]
            _ -> []
      }

makeElementSection ::
     MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeElementSection Module {..} ModuleSymbolTable {..} =
  pure
    Wasm.ElementSection
      { elements =
          case functionTable of
            Just FunctionTable {..} ->
              [ Wasm.Element
                  { tableIndex = Wasm.TableIndex 0
                  , tableOffset =
                      Wasm.Expression
                        {instructions = [Wasm.I32Const {i32ConstValue = 0}]}
                  , tableInitialValues =
                      [functionSymbols ! _func_sym | _func_sym <- functionNames]
                  }
              ]
            _ -> []
      }

data DeBruijnContext = DeBruijnContext
  { currentLevel :: Word32
  , capturedLevels :: Map.Map SBS.ShortByteString Word32
  }

emptyDeBruijnContext :: DeBruijnContext
emptyDeBruijnContext =
  DeBruijnContext {currentLevel = 0, capturedLevels = mempty}

bindLabel :: SBS.ShortByteString -> DeBruijnContext -> DeBruijnContext
bindLabel k DeBruijnContext {..} =
  DeBruijnContext
    { currentLevel = succ currentLevel
    , capturedLevels =
        if SBS.null k
          then capturedLevels
          else Map.insert k currentLevel capturedLevels
    }

extractLabel :: DeBruijnContext -> SBS.ShortByteString -> Wasm.LabelIndex
extractLabel DeBruijnContext {..} k =
  coerce $ currentLevel - capturedLevels ! k - 1

-- TODO: reduce infer usage
makeInstructions ::
     MonadError MarshalError m
  => ModuleSymbolTable
  -> DeBruijnContext
  -> Expression
  -> m (DList.DList Wasm.Instruction)
makeInstructions _module_symtable@ModuleSymbolTable {..} _de_bruijn_ctx expr =
  case expr of
    Block {..} -> do
      let _new_de_bruijn_ctx = bindLabel name _de_bruijn_ctx
      bs <- for bodys $ makeInstructions _module_symtable _new_de_bruijn_ctx
      pure $
        DList.singleton
          Wasm.Block
            { blockResultType = makeResultType valueType
            , blockInstructions = DList.toList $ mconcat bs
            }
    If {..} -> do
      let _new_de_bruijn_ctx = bindLabel mempty _de_bruijn_ctx
      c <- makeInstructions _module_symtable _de_bruijn_ctx condition
      t <-
        DList.toList <$>
        makeInstructions _module_symtable _new_de_bruijn_ctx ifTrue
      f <-
        DList.toList <$>
        makeInstructions _module_symtable _new_de_bruijn_ctx ifFalse
      pure $
        c <>
        DList.singleton
          Wasm.If
            { ifResultType = makeResultType $ infer ifTrue
            , thenInstructions = t
            , elseInstructions =
                case f of
                  [] -> Nothing
                  _ -> Just f
            }
    Loop {..} -> do
      let _new_de_bruijn_ctx = bindLabel name _de_bruijn_ctx
      b <- makeInstructions _module_symtable _new_de_bruijn_ctx body
      pure $
        DList.singleton
          Wasm.Loop {loopResultType = [], loopInstructions = DList.toList b}
    Break {..} -> do
      let _lbl = extractLabel _de_bruijn_ctx name
      case condition of
        Null -> pure $ DList.singleton Wasm.Branch {branchLabel = _lbl}
        _ -> do
          c <- makeInstructions _module_symtable _de_bruijn_ctx condition
          pure $ c <> DList.singleton Wasm.BranchIf {branchIfLabel = _lbl}
    Switch {..} -> do
      c <- makeInstructions _module_symtable _de_bruijn_ctx condition
      pure $
        c <>
        DList.singleton
          Wasm.BranchTable
            { branchTableLabels = map (extractLabel _de_bruijn_ctx) names
            , branchTableFallbackLabel = extractLabel _de_bruijn_ctx defaultName
            }
    Call {..} -> do
      xs <- for operands $ makeInstructions _module_symtable _de_bruijn_ctx
      pure $
        mconcat xs <>
        DList.singleton
          Wasm.Call {callFunctionIndex = functionSymbols ! coerce target}
    CallImport {..} -> do
      xs <- for operands $ makeInstructions _module_symtable _de_bruijn_ctx
      pure $
        mconcat xs <>
        DList.singleton
          Wasm.Call {callFunctionIndex = functionSymbols ! target'}
    CallIndirect {..} -> do
      f <- makeInstructions _module_symtable _de_bruijn_ctx indirectTarget
      xs <- for operands $ makeInstructions _module_symtable _de_bruijn_ctx
      pure $
        mconcat xs <> f <>
        DList.singleton
          Wasm.CallIndirect
            {callIndirectFuctionTypeIndex = functionTypeSymbols ! typeName}
    GetLocal {..} ->
      pure $ DList.singleton Wasm.GetLocal {getLocalIndex = coerce index}
    SetLocal {..} -> do
      v <- makeInstructions _module_symtable _de_bruijn_ctx value
      pure $ v <> DList.singleton Wasm.SetLocal {setLocalIndex = coerce index}
    Load {..} -> do
      let _mem_arg =
            Wasm.MemoryArgument
              {memoryArgumentAlignment = align, memoryArgumentOffset = offset}
      op <-
        DList.singleton <$>
        case (signed, bytes, valueType) of
          (_, 4, I32) -> pure $ Wasm.I32Load _mem_arg
          (_, 8, I64) -> pure $ Wasm.I64Load _mem_arg
          (_, 4, F32) -> pure $ Wasm.F32Load _mem_arg
          (_, 8, F64) -> pure $ Wasm.F64Load _mem_arg
          (True, 1, I32) -> pure $ Wasm.I32Load8Signed _mem_arg
          (False, 1, I32) -> pure $ Wasm.I32Load8Unsigned _mem_arg
          (True, 2, I32) -> pure $ Wasm.I32Load16Signed _mem_arg
          (False, 2, I32) -> pure $ Wasm.I32Load16Unsigned _mem_arg
          (True, 1, I64) -> pure $ Wasm.I64Load8Signed _mem_arg
          (False, 1, I64) -> pure $ Wasm.I64Load8Unsigned _mem_arg
          (True, 2, I64) -> pure $ Wasm.I64Load16Signed _mem_arg
          (False, 2, I64) -> pure $ Wasm.I64Load16Unsigned _mem_arg
          (True, 4, I64) -> pure $ Wasm.I64Load32Signed _mem_arg
          (False, 4, I64) -> pure $ Wasm.I64Load32Unsigned _mem_arg
          _ -> throwError $ UnsupportedExpression expr
      p <- makeInstructions _module_symtable _de_bruijn_ctx ptr
      pure $ p <> op
    Store {..} -> do
      let _mem_arg =
            Wasm.MemoryArgument
              {memoryArgumentAlignment = align, memoryArgumentOffset = offset}
      op <-
        DList.singleton <$>
        case (bytes, valueType) of
          (4, I32) -> pure $ Wasm.I32Store _mem_arg
          (8, I64) -> pure $ Wasm.I64Store _mem_arg
          (4, F32) -> pure $ Wasm.F32Store _mem_arg
          (8, F64) -> pure $ Wasm.F64Store _mem_arg
          (1, I32) -> pure $ Wasm.I32Store8 _mem_arg
          (2, I32) -> pure $ Wasm.I32Store16 _mem_arg
          (1, I64) -> pure $ Wasm.I64Store8 _mem_arg
          (2, I64) -> pure $ Wasm.I64Store16 _mem_arg
          (4, I64) -> pure $ Wasm.I64Store32 _mem_arg
          _ -> throwError $ UnsupportedExpression expr
      p <- makeInstructions _module_symtable _de_bruijn_ctx ptr
      v <- makeInstructions _module_symtable _de_bruijn_ctx value
      pure $ p <> v <> op
    ConstI32 v -> pure $ DList.singleton Wasm.I32Const {i32ConstValue = v}
    ConstI64 v -> pure $ DList.singleton Wasm.I64Const {i64ConstValue = v}
    ConstF32 v -> pure $ DList.singleton Wasm.F32Const {f32ConstValue = v}
    ConstF64 v -> pure $ DList.singleton Wasm.F64Const {f64ConstValue = v}
    Unary {..} -> do
      x <- makeInstructions _module_symtable _de_bruijn_ctx operand0
      op <-
        DList.singleton <$>
        case unaryOp of
          ClzInt32 -> pure Wasm.I32Clz
          CtzInt32 -> pure Wasm.I32Ctz
          PopcntInt32 -> pure Wasm.I32Popcnt
          NegFloat32 -> pure Wasm.F32Neg
          AbsFloat32 -> pure Wasm.F32Abs
          CeilFloat32 -> pure Wasm.F32Ceil
          FloorFloat32 -> pure Wasm.F32Floor
          TruncFloat32 -> pure Wasm.F32Trunc
          NearestFloat32 -> pure Wasm.F32Nearest
          SqrtFloat32 -> pure Wasm.F32Sqrt
          EqZInt32 -> pure Wasm.I32Eqz
          ClzInt64 -> pure Wasm.I64Clz
          CtzInt64 -> pure Wasm.I64Ctz
          PopcntInt64 -> pure Wasm.I64Popcnt
          NegFloat64 -> pure Wasm.F64Neg
          AbsFloat64 -> pure Wasm.F64Abs
          CeilFloat64 -> pure Wasm.F64Ceil
          FloorFloat64 -> pure Wasm.F64Floor
          TruncFloat64 -> pure Wasm.F64Trunc
          NearestFloat64 -> pure Wasm.F64Nearest
          SqrtFloat64 -> pure Wasm.F64Sqrt
          EqZInt64 -> pure Wasm.I64Eqz
          ExtendSInt32 -> pure Wasm.I64ExtendSFromI32
          ExtendUInt32 -> pure Wasm.I64ExtendUFromI32
          WrapInt64 -> pure Wasm.I32WrapFromI64
          TruncSFloat32ToInt32 -> pure Wasm.I32TruncSFromF32
          TruncSFloat32ToInt64 -> pure Wasm.I64TruncSFromF32
          TruncUFloat32ToInt32 -> pure Wasm.I32TruncUFromF32
          TruncUFloat32ToInt64 -> pure Wasm.I64TruncUFromF32
          TruncSFloat64ToInt32 -> pure Wasm.I32TruncSFromF64
          TruncSFloat64ToInt64 -> pure Wasm.I64TruncSFromF64
          TruncUFloat64ToInt32 -> pure Wasm.I32TruncUFromF64
          TruncUFloat64ToInt64 -> pure Wasm.I64TruncUFromF64
          ReinterpretFloat32 -> pure Wasm.I32ReinterpretFromF32
          ReinterpretFloat64 -> pure Wasm.I64ReinterpretFromF64
          ConvertSInt32ToFloat32 -> pure Wasm.F32ConvertSFromI32
          ConvertSInt32ToFloat64 -> pure Wasm.F64ConvertSFromI32
          ConvertUInt32ToFloat32 -> pure Wasm.F32ConvertUFromI32
          ConvertUInt32ToFloat64 -> pure Wasm.F64ConvertUFromI32
          ConvertSInt64ToFloat32 -> pure Wasm.F32ConvertSFromI64
          ConvertSInt64ToFloat64 -> pure Wasm.F64ConvertSFromI64
          ConvertUInt64ToFloat32 -> pure Wasm.F32ConvertUFromI64
          ConvertUInt64ToFloat64 -> pure Wasm.F64ConvertUFromI64
          PromoteFloat32 -> pure Wasm.F64PromoteFromF32
          DemoteFloat64 -> pure Wasm.F32DemoteFromF64
          ReinterpretInt32 -> pure Wasm.F32ReinterpretFromI32
          ReinterpretInt64 -> pure Wasm.F64ReinterpretFromI64
      pure $ x <> op
    Binary {..} -> do
      x <- makeInstructions _module_symtable _de_bruijn_ctx operand0
      y <- makeInstructions _module_symtable _de_bruijn_ctx operand1
      op <-
        DList.singleton <$>
        case binaryOp of
          AddInt32 -> pure Wasm.I32Add
          SubInt32 -> pure Wasm.I32Sub
          MulInt32 -> pure Wasm.I32Mul
          DivSInt32 -> pure Wasm.I32DivS
          DivUInt32 -> pure Wasm.I32DivU
          RemSInt32 -> pure Wasm.I32RemS
          RemUInt32 -> pure Wasm.I32RemU
          AndInt32 -> pure Wasm.I32And
          OrInt32 -> pure Wasm.I32Or
          XorInt32 -> pure Wasm.I32Xor
          ShlInt32 -> pure Wasm.I32Shl
          ShrUInt32 -> pure Wasm.I32ShrU
          ShrSInt32 -> pure Wasm.I32ShrS
          RotLInt32 -> pure Wasm.I32RotL
          RotRInt32 -> pure Wasm.I32RotR
          EqInt32 -> pure Wasm.I32Eq
          NeInt32 -> pure Wasm.I32Ne
          LtSInt32 -> pure Wasm.I32LtS
          LtUInt32 -> pure Wasm.I32LtU
          LeSInt32 -> pure Wasm.I32LeS
          LeUInt32 -> pure Wasm.I32LeU
          GtSInt32 -> pure Wasm.I32GtS
          GtUInt32 -> pure Wasm.I32GtU
          GeSInt32 -> pure Wasm.I32GeS
          GeUInt32 -> pure Wasm.I32GeU
          AddInt64 -> pure Wasm.I64Add
          SubInt64 -> pure Wasm.I64Sub
          MulInt64 -> pure Wasm.I64Mul
          DivSInt64 -> pure Wasm.I64DivS
          DivUInt64 -> pure Wasm.I64DivU
          RemSInt64 -> pure Wasm.I64RemS
          RemUInt64 -> pure Wasm.I64RemU
          AndInt64 -> pure Wasm.I64And
          OrInt64 -> pure Wasm.I64Or
          XorInt64 -> pure Wasm.I64Xor
          ShlInt64 -> pure Wasm.I64Shl
          ShrUInt64 -> pure Wasm.I64ShrU
          ShrSInt64 -> pure Wasm.I64ShrS
          RotLInt64 -> pure Wasm.I64RotL
          RotRInt64 -> pure Wasm.I64RotR
          EqInt64 -> pure Wasm.I64Eq
          NeInt64 -> pure Wasm.I64Ne
          LtSInt64 -> pure Wasm.I64LtS
          LtUInt64 -> pure Wasm.I64LtU
          LeSInt64 -> pure Wasm.I64LeS
          LeUInt64 -> pure Wasm.I64LeU
          GtSInt64 -> pure Wasm.I64GtS
          GtUInt64 -> pure Wasm.I64GtU
          GeSInt64 -> pure Wasm.I64GeS
          GeUInt64 -> pure Wasm.I64GeU
          AddFloat32 -> pure Wasm.F32Add
          SubFloat32 -> pure Wasm.F32Sub
          MulFloat32 -> pure Wasm.F32Mul
          DivFloat32 -> pure Wasm.F32Div
          CopySignFloat32 -> pure Wasm.F32Copysign
          MinFloat32 -> pure Wasm.F32Min
          MaxFloat32 -> pure Wasm.F32Max
          EqFloat32 -> pure Wasm.F32Eq
          NeFloat32 -> pure Wasm.F32Ne
          LtFloat32 -> pure Wasm.F32Lt
          LeFloat32 -> pure Wasm.F32Le
          GtFloat32 -> pure Wasm.F32Gt
          GeFloat32 -> pure Wasm.F32Ge
          AddFloat64 -> pure Wasm.F64Add
          SubFloat64 -> pure Wasm.F64Sub
          MulFloat64 -> pure Wasm.F64Mul
          DivFloat64 -> pure Wasm.F64Div
          CopySignFloat64 -> pure Wasm.F64Copysign
          MinFloat64 -> pure Wasm.F64Min
          MaxFloat64 -> pure Wasm.F64Max
          EqFloat64 -> pure Wasm.F64Eq
          NeFloat64 -> pure Wasm.F64Ne
          LtFloat64 -> pure Wasm.F64Lt
          LeFloat64 -> pure Wasm.F64Le
          GtFloat64 -> pure Wasm.F64Gt
          GeFloat64 -> pure Wasm.F64Ge
      pure $ x <> y <> op
    Host {..} -> do
      let op =
            DList.singleton $
            case hostOp of
              CurrentMemory -> Wasm.MemorySize
              GrowMemory -> Wasm.MemoryGrow
      xs <- for operands $ makeInstructions _module_symtable _de_bruijn_ctx
      pure $ mconcat xs <> op
    Nop -> pure $ DList.singleton Wasm.Nop
    Unreachable -> pure $ DList.singleton Wasm.Unreachable
    Null -> pure mempty
    _ -> throwError $ UnsupportedExpression expr

makeCodeSection ::
     MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeCodeSection Module {..} _module_symtable =
  fmap Wasm.CodeSection $
  for (Map.elems functionMap') $ \Function {..} -> do
    _locals <-
      for varTypes $ \case
        None -> throwError InvalidLocalType
        I32 -> pure Wasm.I32
        I64 -> pure Wasm.I64
        F32 -> pure Wasm.F32
        F64 -> pure Wasm.F64
    _body <- makeInstructions _module_symtable emptyDeBruijnContext body
    pure
      Wasm.Function
        { functionLocals =
            [Wasm.Locals {localsCount = 1, localsType = vt} | vt <- _locals]
        , functionBody = coerce $ DList.toList _body
        }

makeDataSection ::
     MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeDataSection Module {..} _module_symtable =
  case memory of
    Just Memory {..} -> do
      segs <-
        for dataSegments $ \DataSegment {..} -> do
          instrs <-
            makeInstructions _module_symtable emptyDeBruijnContext offset
          pure
            Wasm.DataSegment
              { memoryIndex = Wasm.MemoryIndex 0
              , memoryOffset = coerce $ DList.toList instrs
              , memoryInitialBytes = content
              }
      pure Wasm.DataSection {dataSegments = segs}
    _ -> pure Wasm.DataSection {dataSegments = []}

makeModule :: MonadError MarshalError m => Module -> m Wasm.Module
makeModule m = do
  _module_symtable <- makeModuleSymbolTable m
  _type_sec <- makeTypeSection m
  _import_sec <- makeImportSection m _module_symtable
  _func_sec <- makeFunctionSection m _module_symtable
  _table_sec <- makeTableSection m
  _mem_sec <- makeMemorySection m
  _export_sec <- makeExportSection m _module_symtable
  _elem_sec <- makeElementSection m _module_symtable
  _code_sec <- makeCodeSection m _module_symtable
  _data_sec <- makeDataSection m _module_symtable
  pure $
    Wasm.Module
      [ _type_sec
      , _import_sec
      , _func_sec
      , _table_sec
      , _mem_sec
      , _export_sec
      , _elem_sec
      , _code_sec
      , _data_sec
      ]
