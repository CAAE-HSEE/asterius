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
  , globalSymbols :: Map.Map SBS.ShortByteString Wasm.GlobalIndex
  }

makeModuleSymbolTable ::
     MonadError MarshalError m => Module -> m ModuleSymbolTable
makeModuleSymbolTable Module {..} = do
  let _has_dup l = length l /= length (nub l)
      _func_import_syms =
        [internalName | FunctionImport {..} <- functionImports]
      _func_syms = Map.keys functionMap'
      _func_conflict_syms = _func_import_syms `intersect` _func_syms
      _global_import_syms = [internalName | GlobalImport {..} <- globalImports]
      _global_syms = Map.keys globalMap
      _global_conflict_syms = _global_import_syms `intersect` _global_syms
  if _has_dup _func_import_syms
    then throwError DuplicateFunctionImport
    else if _has_dup _global_import_syms
           then throwError DuplicateGlobalImport
           else pure
                  ModuleSymbolTable
                    { functionTypeSymbols =
                        Map.fromDistinctAscList $
                        zip (Map.keys functionTypeMap) (coerce [0 :: Word32 ..])
                    , functionSymbols =
                        Map.fromDistinctAscList $
                        zip
                          (_func_import_syms <> _func_syms)
                          (coerce [0 :: Word32 ..])
                    , globalSymbols =
                        Map.fromDistinctAscList $
                        zip
                          (_global_import_syms <> _global_syms)
                          (coerce [0 :: Word32 ..])
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
    Switch {..} -> undefined
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
      op <- DList.singleton <$> undefined
      p <- makeInstructions _module_symtable _de_bruijn_ctx ptr
      pure $ p <> op
    Store {..} -> do
      op <- DList.singleton <$> undefined
      p <- makeInstructions _module_symtable _de_bruijn_ctx ptr
      v <- makeInstructions _module_symtable _de_bruijn_ctx value
      pure $ p <> v <> op
    ConstI32 v -> pure $ DList.singleton Wasm.I32Const {i32ConstValue = v}
    ConstI64 v -> pure $ DList.singleton Wasm.I64Const {i64ConstValue = v}
    ConstF32 v -> pure $ DList.singleton Wasm.F32Const {f32ConstValue = v}
    ConstF64 v -> pure $ DList.singleton Wasm.F64Const {f64ConstValue = v}
    Unary {..} -> do
      x <- makeInstructions _module_symtable _de_bruijn_ctx operand0
      op <- DList.singleton <$> undefined
      pure $ x <> op
    Binary {..} -> do
      x <- makeInstructions _module_symtable _de_bruijn_ctx operand0
      y <- makeInstructions _module_symtable _de_bruijn_ctx operand1
      op <- DList.singleton <$> undefined
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
