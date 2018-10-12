{-# LANGUAGE RecordWildCards #-}

module ShrinkModule
  ( shrinkExpression
  , shrinkModule
  ) where

import qualified Asterius.Internals.FList as FList
import Asterius.Types
import qualified Data.Map.Strict as Map
import GHC.Exts

type Shrink a = a -> FList.FList a

preserve :: a -> Shrink a -> Shrink a
preserve def f a = FList.cons def r
  where
    r = f a

shrinkExpression :: Shrink Expression
shrinkExpression =
  preserve Unreachable $ \expr ->
    case expr of
      Block {..} -> do
        _new_bodys <- traverse shrinkExpression bodys
        pure expr {bodys = _new_bodys}
      If {..} -> do
        _new_ifTrue <- shrinkExpression ifTrue
        _new_ifFalse <- shrinkExpression ifFalse
        pure expr {ifTrue = _new_ifTrue, ifFalse = _new_ifFalse}
      Loop {..} -> do
        _new_body <- shrinkExpression body
        pure Loop {name = name, body = _new_body}
      Break {..} -> do
        _new_condition <- shrinkExpression condition
        pure Break {name = name, condition = _new_condition}
      Switch {..} -> do
        _new_condition <- shrinkExpression condition
        pure
          Switch
            { names = names
            , defaultName = defaultName
            , condition = _new_condition
            }
      Call {..} -> do
        _new_operands <- traverse shrinkExpression operands
        pure expr {operands = _new_operands}
      CallImport {..} -> do
        _new_operands <- traverse shrinkExpression operands
        pure expr {operands = _new_operands}
      CallIndirect {..} -> do
        _new_indirect_target <- shrinkExpression indirectTarget
        _new_operands <- traverse shrinkExpression operands
        pure
          expr {indirectTarget = _new_indirect_target, operands = _new_operands}
      SetLocal {..} -> do
        _new_value <- shrinkExpression value
        pure expr {value = _new_value}
      Load {..} -> do
        _new_ptr <- shrinkExpression ptr
        pure expr {ptr = _new_ptr}
      Store {..} -> do
        _new_ptr <- shrinkExpression ptr
        _new_value <- shrinkExpression value
        pure expr {ptr = _new_ptr, value = _new_value}
      Unary {..} -> do
        _new_operand0 <- shrinkExpression operand0
        pure expr {operand0 = _new_operand0}
      Binary {..} -> do
        _new_operand0 <- shrinkExpression operand0
        _new_operand1 <- shrinkExpression operand1
        pure expr {operand0 = _new_operand0, operand1 = _new_operand1}
      Host {..} -> do
        _new_operands <- traverse shrinkExpression operands
        pure expr {operands = _new_operands}
      _ -> mempty

shrinkModule' :: Shrink Module
shrinkModule' m@Module {..} = do
  let (_function_map_no_shrink, _function_map_to_shrink) =
        Map.partition
          (\Function {..} ->
             case body of
               Unreachable -> True
               _ -> False)
          functionMap'
  (_func_to_shrink_key, Function {..}) <-
    fromList $ toList _function_map_to_shrink
  _shrink_expr <- shrinkExpression body
  pure
    m
      { functionMap' =
          _function_map_no_shrink <>
          Map.insert
            _func_to_shrink_key
            Function
              { functionTypeName = functionTypeName
              , varTypes = varTypes
              , body = _shrink_expr
              }
            _function_map_to_shrink
      }

shrinkModule :: Module -> [Module]
shrinkModule = toList . shrinkModule'
