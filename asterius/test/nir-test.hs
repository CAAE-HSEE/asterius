{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

import Asterius.Marshal
import Asterius.Types
import Bindings.Binaryen.Raw
import Foreign

main :: IO ()
main = do
  c_BinaryenSetAPITracing 1
  m <-
    withPool $ \pool ->
      marshalModule pool $
      Module
        { functionTypeMap = [("func_type", FunctionType I32 [])]
        , functionMap' =
            [ ( "func"
              , Function "func_type" [I32] $
                CFG
                  { graph =
                      RelooperRun
                        { entry = ".entry"
                        , blockMap =
                            [ ( ".entry"
                              , RelooperBlock
                                  { addBlock =
                                      AddBlockWithSwitch Nop (GetLocal 0 I32)
                                  , addBranches =
                                      [ AddBranchForSwitch ".odd" [0] Null
                                      , AddBranch ".def" Null Null
                                      ]
                                  })
                            , ( ".odd"
                              , RelooperBlock
                                  { addBlock = AddBlock $ ConstI32 19
                                  , addBranches = []
                                  })
                            , ( ".def"
                              , RelooperBlock
                                  { addBlock = AddBlock $ ConstI32 233
                                  , addBranches = []
                                  })
                            ]
                        , labelHelper = 0
                        }
                  })
            ]
        , functionImports = []
        , functionExports = []
        , functionTable = FunctionTable []
        , memory = Memory 1 1 mempty mempty
        }
  c_BinaryenModulePrint m
  c_BinaryenModuleValidate m >>= print
  c_BinaryenModuleDispose m
