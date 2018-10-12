import Asterius.Internals
import qualified Asterius.Marshal as OldMarshal
import qualified Asterius.NewMarshal as NewMarshal
import Asterius.Types
import Control.Exception
import Control.Monad.Except
import Data.Binary (encode)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Foreign
import qualified Language.WebAssembly.WireFormat as Wasm
import qualified Prelude
import Prelude hiding (IO)
import ShrinkModule
import System.Directory
import System.Exit
import System.FilePath
import System.IO hiding (IO)
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic

type Backend = (String, Module -> Prelude.IO LBS.ByteString)

binaryenBackend, wasmToolkitBackend :: Backend
binaryenBackend =
  ( "binaryen"
  , \m ->
      withPool $ \pool ->
        fmap LBS.fromStrict $
        OldMarshal.marshalModule pool m >>= OldMarshal.serializeModule)

wasmToolkitBackend =
  ( "wasm-toolkit"
  , \m ->
      case runExcept (NewMarshal.makeModule m) of
        Left err -> throwIO err
        Right r -> pure $ runPut $ Wasm.putModule r)

testNodeCompile :: Backend -> Module -> Property
testNodeCompile (backend_tag, backend) m =
  monadicIO $ do
    _result <-
      run $ do
        tmpdir <- getTemporaryDirectory
        bracket
          (openBinaryTempFile tmpdir "wasm-toolkit-test.wasm")
          (\(p, _) -> removeFile p)
          (\(p, h) -> do
             buf <- backend m
             LBS.hPut h buf
             hClose h
             (_exit_code, _stdout, _stderr) <-
               readProcessWithExitCode
                 "node"
                 ["test" </> "node-compile" </> "node-compile.js", p]
                 ""
             case _exit_code of
               ExitSuccess -> pure Nothing
               _ -> do
                 (p_err, h_err) <-
                   openTempFile tmpdir "wasm-toolkit-test-dump.bin"
                 LBS.hPut h_err $ encode m
                 hClose h_err
                 pure $ Just (_exit_code, _stdout, _stderr, p_err))
    case _result of
      Just (_exit_code, _stdout, _stderr, p_err) ->
        fail $
        "Compiling serialized module via " <> backend_tag <>
        " backend failed.\nExit code: " <>
        show _exit_code <>
        "\nStdout: " <>
        _stdout <>
        "\nStderr: " <>
        _stderr <>
        "\nModule binary dump path: " <>
        p_err
      _ -> pure ()

testNodeCompileWithShrink ::
     Backend -> (Module -> [Module]) -> Module -> Property
testNodeCompileWithShrink backend s m = shrinking s m $ testNodeCompile backend

testNodeCompileBoth :: (Module -> [Module]) -> Module -> Property
testNodeCompileBoth s m =
  testNodeCompileWithShrink binaryenBackend s m .&&.
  testNodeCompileWithShrink wasmToolkitBackend s m

main :: IO ()
main = do
  m_fib <- decodeFile $ "test" </> "fib" </> "fib.bin"
  quickCheck $ testNodeCompileBoth shrinkModule m_fib
