module Main (main) where

import Control.Monad.Trans
import Data.Set (fromList)
import HW5.Action
import HW5.Evaluator
import HW5.Parser
import HW5.Pretty
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import System.Console.Haskeline
import System.IO
import Text.Megaparsec.Error (errorBundlePretty)


-- | Settings for the Haskeline library.
mySettings :: Settings IO
mySettings = defaultSettings {historyFile = Just "history"}

-- | Prints Prettyprinter 'Doc' to stdout.
render :: Doc AnsiStyle -> IO ()
render = renderIO System.IO.stdout . layoutSmart defaultLayoutOptions

main :: IO ()
main = do
  runInputT mySettings loop

-- | The main loop of the REPL.
loop :: InputT IO ()
loop = do
  maybeLine <- getInputLine "hi> "
  case maybeLine of
    Just inputLine -> do
      case parse inputLine of
        Right expr -> do
          let permissions = fromList [AllowRead, AllowWrite, AllowTime]
          result <- lift $ runHIO (eval expr) permissions
          lift $ render $ vsep [prettyExpr expr, emptyDoc, case result of
              Left err  -> pretty "Eval error:" <+> viaShow err
              Right val -> prettyValue val, emptyDoc]
        Left err -> lift $ render $ vsep [pretty "Parse error:", pretty $ errorBundlePretty err]
      loop
    Nothing -> return ()
