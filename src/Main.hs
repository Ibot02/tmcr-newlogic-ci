{-# LANGUAGE LambdaCase #-}
module Main where

import Data.TMCR.Logic.Parser

import System.IO
import System.Environment
import System.Exit

import qualified Text.Megaparsec as P
import Control.Monad.Reader

import Data.Void

main :: IO ()
main = do
        inputs <- getArgs
        successes <- forM inputs $ \i -> do
            runOnFile i
        case (and successes) of
            True -> putStrLn $ "Parsed " <> show (length inputs) <> " files without any errors."
            False -> do
                hPutStrLn stderr "Errors were encountered when trying to parse the following files:"
                forM_ (filter (not . snd) $ zip inputs successes) $ \(file, _) ->
                    hPutStrLn stderr file
                exitFailure


runOnFile :: String -> IO Bool
runOnFile i = parseFile i >>= \case
                Left err -> do
                    hPutStrLn stderr $ P.errorBundlePretty err
                    return False
                Right _ -> return True

parseFile :: String -> IO (Either (P.ParseErrorBundle String Void) Forest)
parseFile "-" = runOnHandle "stdin" stdin
parseFile i = withFile i ReadMode $ \h -> runOnHandle i h >>= \case
                                              Left err -> return $ Left err
                                              Right s -> return $ Right s

runOnHandle :: String -> Handle -> IO (Either (P.ParseErrorBundle String Void) Forest)
runOnHandle name h = P.parse (runReaderT (logicParser ["area", "room"]) logicTypedefs <* P.eof) name <$> hGetContents h

logicTypedefs = [ SugarOpList "and" "&"
                , SugarOpList "or"  "|"
                ]
