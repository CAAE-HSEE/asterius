{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

import Asterius.Boot
import Asterius.BuildInfo
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals
import Asterius.JSFFI
import qualified Asterius.Marshal as OldMarshal
import qualified Asterius.NewMarshal as NewMarshal
import Asterius.Resolve
import Asterius.Store
import Asterius.Types
import Bindings.Binaryen.Raw
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Foreign
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.BuildInfo (ahcGccPath)
import Language.Haskell.GHC.Toolkit.Constants
import Language.Haskell.GHC.Toolkit.Run hiding (ghcLibDir)
import Language.WebAssembly.WireFormat
import Options.Applicative
import Prelude hiding (IO)
import System.Directory
import System.FilePath
import System.IO hiding (IO)
import System.Process

data Target
  = Node
  | Browser
  deriving (Eq)

data Task = Task
  { target :: Target
  , input, outputWasm, outputJS :: FilePath
  , outputLinkReport, outputGraphViz :: Maybe FilePath
  , wasmToolkit, debug, optimize, outputIR, run :: Bool
  , heapSize :: Int
  , asteriusInstanceCallback :: String
  , extraGHCFlags :: [String]
  , exportFunctions, extraRootSymbols :: [AsteriusEntitySymbol]
  }

parseTask :: Parser Task
parseTask =
  (\t i m_wasm m_node m_report m_gv wasm_toolkit dbg opt ir r m_hs m_with_i ghc_flags export_funcs root_syms ->
     Task
       { target = t
       , input = i
       , outputWasm = fromMaybe (i -<.> "wasm") m_wasm
       , outputJS = fromMaybe (i -<.> "js") m_node
       , outputLinkReport = m_report
       , outputGraphViz = m_gv
       , wasmToolkit = wasm_toolkit
       , debug = dbg
       , optimize = opt && not dbg
       , outputIR = ir || dbg
       , run = r
       , heapSize = maybe 1024 read m_hs
       , asteriusInstanceCallback =
           fromMaybe
             "i => {\ni.wasmInstance.exports.hs_init();\ni.wasmInstance.exports.rts_evalLazyIO(i.staticsSymbolMap.MainCapability, i.staticsSymbolMap.Main_main_closure, 0);\n}"
             m_with_i
       , extraGHCFlags = ghc_flags
       , exportFunctions =
           [AsteriusEntitySymbol {entityName = sym} | sym <- export_funcs]
       , extraRootSymbols =
           [AsteriusEntitySymbol {entityName = sym} | sym <- root_syms]
       }) <$>
  fmap
    (\f ->
       if f
         then Browser
         else Node)
    (switch (long "browser" <> help "Target browsers instead of Node.js")) <*>
  strOption (long "input" <> help "Path of the Main module") <*>
  optional
    (strOption
       (long "output-wasm" <>
        help "Output path of WebAssembly binary, defaults to same path of Main")) <*>
  optional
    (strOption
       (long "output-js" <>
        help
          "Output path of JavaScript, defaults to same path of Main. Must be the same directory as the WebAssembly binary.")) <*>
  optional
    (strOption
       (long "output-link-report" <> help "Output path of linking report")) <*>
  optional
    (strOption
       (long "output-graphviz" <>
        help "Output path of GraphViz file of symbol dependencies")) <*>
  switch
    (long "wasm-toolkit" <> help "Enable experimental wasm-toolkit backend") <*>
  switch (long "debug" <> help "Enable debug mode in the runtime") <*>
  switch (long "optimize" <> help "Enable V8 optimization") <*>
  switch (long "output-ir" <> help "Output Asterius IR of compiled modules") <*>
  switch (long "run" <> help "Run the compiled module with Node.js") <*>
  optional
    (strOption
       (long "heap-size" <>
        help
          "Heap size in MBs, used for both nursery/object pool. Defaults to 1024.")) <*>
  optional
    (strOption
       (long "asterius-instance-callback" <>
        help
          "Supply a JavaScript callback expression which will be invoked on the initiated asterius instance. Defaults to calling Main.main")) <*>
  many (strOption (long "ghc-option" <> help "Extra GHC flags")) <*>
  many
    (strOption (long "export-function" <> help "Symbol of exported function")) <*>
  many
    (strOption
       (long "extra-root-symbol" <>
        help "Symbol of extra root entity, e.g. Main_f_closure"))

opts :: ParserInfo Task
opts =
  info
    (parseTask <**> helper)
    (fullDesc <>
     progDesc "Producing a standalone WebAssembly binary from Haskell" <>
     header "ahc-link - Linker for the Asterius compiler")

genSymbolDict :: M.Map AsteriusEntitySymbol Int64 -> Builder
genSymbolDict sym_map =
  "{" <>
  mconcat
    (intersperse
       ","
       [ string7 (show sym) <> ":" <> int64Dec sym_idx
       | (sym, sym_idx) <- M.toList sym_map
       ]) <>
  "}"

genNode :: Task -> LinkReport -> [ErrorMessage] -> IO Builder
genNode Task {..} LinkReport {..} err_msgs = do
  rts_buf <- BS.readFile $ dataDir </> "rts" </> "rts.js"
  pure $
    mconcat $
    [ byteString rts_buf
    , "let __asterius_instance = null;\n"
    , "async function main() {\n"
    , "const i = await newAsteriusInstance({functionSymbols: "
    , string7 $ show $ map fst $ sortOn snd $ M.toList functionSymbolMap
    , ", errorMessages: ["
    , mconcat (intersperse "," [string7 $ show msg | msg <- err_msgs])
    , "], bufferSource: "
    ] <>
    (case target of
       Node ->
         [ "await require(\"fs\").promises.readFile("
         , string7 $ show $ takeFileName outputWasm
         , ")"
         ]
       Browser -> ["fetch(", string7 $ show $ takeFileName outputWasm, ")"]) <>
    [ ", jsffiFactory: "
    , generateFFIImportObjectFactory bundledFFIMarshalState
    , ", staticsSymbolMap: "
    , genSymbolDict staticsSymbolMap
    , ", functionSymbolMap: "
    , genSymbolDict functionSymbolMap
    , "});\n"
    , "__asterius_instance = i\n;"
    , "("
    , string7 asteriusInstanceCallback
    , ")(i);\n"
    , "}\n"
    , case target of
        Node -> "process.on('unhandledRejection', err => { throw err; });\n"
        Browser -> mempty
    , "main();\n"
    ]

main :: IO ()
main = do
  task@Task {..} <- execParser opts
  (boot_store, boot_pkgdb) <-
    do (store_path, boot_pkgdb) <-
         do boot_args <- getDefaultBootArgs
            let boot_lib = bootDir boot_args </> "asterius_lib"
            pure (boot_lib </> "asterius_store", boot_lib </> "package.conf.d")
       putStrLn $ "[INFO] Loading boot library store from " <> show store_path
       store <- decodeStore store_path
       pure (store, boot_pkgdb)
  putStrLn "[INFO] Populating the store with builtin routines"
  def_builtins_opts <- getDefaultBuiltinsOptions
  let builtins_opts =
        def_builtins_opts
          {nurseryGroups = blocks_per_mblock * heapSize, tracing = debug}
      !orig_store = builtinsStore builtins_opts <> boot_store
  putStrLn $ "[INFO] Compiling " <> input <> " to Cmm"
  (c, get_ffi_mod) <- addFFIProcessor mempty
  mod_ir_map <-
    runHaskell
      defaultConfig
        { ghcFlags =
            [ "-Wall"
            , "-O"
            , "-i" <> takeDirectory input
            , "-clear-package-db"
            , "-global-package-db"
            , "-package-db"
            , boot_pkgdb
            , "-hide-all-packages"
            ] <>
            mconcat
              [ ["-package", pkg]
              | pkg <-
                  [ "ghc-prim"
                  , "integer-simple"
                  , "base"
                  , "array"
                  , "deepseq"
                  , "containers"
                  , "transformers"
                  , "mtl"
                  , "pretty"
                  , "ghc-boot-th"
                  , "template-haskell"
                  ]
              ] <>
            ["-pgmc=" <> ahcGccPath] <>
            extraGHCFlags
        , compiler = c
        }
      [input]
  putStrLn "[INFO] Marshalling from Cmm to WebAssembly"
  final_store_ref <- newIORef orig_store
  M.foldlWithKey'
    (\act ms_mod ir ->
       case runCodeGen (marshalHaskellIR ir) (dflags builtins_opts) ms_mod of
         Left err -> throwIO err
         Right m' -> do
           let mod_sym = marshalToModuleSymbol ms_mod
               mod_str = GHC.moduleNameString $ GHC.moduleName ms_mod
           ffi_mod <- get_ffi_mod mod_sym
           let m = ffi_mod <> m'
           putStrLn $
             "[INFO] Marshalling " <> show mod_str <> " from Cmm to WebAssembly"
           modifyIORef' final_store_ref $
             addModule (marshalToModuleSymbol ms_mod) m
           when outputIR $ do
             let p = takeDirectory input </> mod_str <.> "txt"
             putStrLn $ "[INFO] Writing IR of " <> mod_str <> " to " <> p
             writeFile p $ show m
           act)
    (pure ())
    mod_ir_map
  final_store <- readIORef final_store_ref
  putStrLn "[INFO] Attempting to link into a standalone WebAssembly module"
  (!m_final_m, !report) <-
    linkStart
      debug
      final_store
      (S.fromList $
       extraRootSymbols <>
       [ AsteriusEntitySymbol {entityName = internalName}
       | FunctionExport {..} <- rtsAsteriusFunctionExports debug
       ])
      exportFunctions
  maybe
    (pure ())
    (\p -> do
       putStrLn $ "[INFO] Writing linking report to " <> show p
       writeFile p $ show report)
    outputLinkReport
  maybe
    (pure ())
    (\p -> do
       putStrLn $
         "[INFO] Writing GraphViz file of symbol dependencies to " <> show p
       writeDot p report)
    outputGraphViz
  maybe
    (fail "[ERROR] Linking failed")
    (\(final_m, err_msgs) -> do
       when outputIR $ do
         let p = input -<.> "bin"
         putStrLn $ "[INFO] Serializing linked IR to " <> show p
         encodeFile p final_m
       if wasmToolkit
         then do
           putStrLn "[INFO] Converting linked IR to wasm-toolkit IR"
           let conv_result = runExcept $ NewMarshal.makeModule final_m
           r <-
             case conv_result of
               Left err -> fail $ "[ERROR] Conversion failed with " <> show err
               Right r -> pure r
           when outputIR $ do
             let p = input -<.> "wasm.txt"
             putStrLn $ "[INFO] Writing wasm-toolkit IR to " <> show p
             writeFile p $ show r
           putStrLn $ "[INFO] Writing WebAssembly binary to " <> show outputWasm
           LBS.writeFile outputWasm $ runPut $ putModule r
         else do
           putStrLn "[INFO] Converting linked IR to binaryen IR"
           m_ref <- withPool $ \pool -> OldMarshal.marshalModule pool final_m
           putStrLn "[INFO] Validating binaryen IR"
           pass_validation <- c_BinaryenModuleValidate m_ref
           when (pass_validation /= 1) $
             fail "[ERROR] binaryen validation failed"
           putStrLn $ "[INFO] Writing WebAssembly binary to " <> show outputWasm
           m_bin <- OldMarshal.serializeModule m_ref
           BS.writeFile outputWasm m_bin
       putStrLn $ "[INFO] Writing JavaScript to " <> show outputJS
       h <- openBinaryFile outputJS WriteMode
       b <- genNode task report err_msgs
       hPutBuilder h b
       hClose h
       when (target == Node && run) $ do
         putStrLn $ "[INFO] Running " <> outputJS
         withCurrentDirectory (takeDirectory outputWasm) $
           callProcess "node" $
           ["--wasm-opt" | optimize] <>
           ["--harmony-bigint", takeFileName outputJS])
    m_final_m
