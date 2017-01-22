#!/usr/bin/env stack
-- stack --resolver lts-7.16 --install-ghc runghc --package text
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Data.Text (Text)
import System.Environment
import System.IO

import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

-- | Which files to include into template
files :: [FilePath]
files = [
    ".gitignore"
  , "LICENSE"
  , "README.md"
  , "Setup.hs"
  , "gore-and-ash-sample.cabal"
  , "gore-and-ash-sample.sublime-project"
  , "stack.yaml"
  , "examples/Example01.hs"
  , "src/Game/GoreAndAsh/ModuleSample/API.hs"
  , "src/Game/GoreAndAsh/ModuleSample/Module.hs"
  , "src/Game/GoreAndAsh/ModuleSample.hs"
  ]

-- | Pairs of replaces, first element
replaces :: [(Text, Text)]
replaces = [
    ("gore-and-ash-sample", "{{name}}")
  , ("ModuleSample", "{{module-name}}")
  , (" sample ", " {{module-docs-name}} ")
  , ("moduleSample", "{{field-prefix}}")
  , ("github-username", "{{github-username}}")
  , ("author-name", "{{author-name}}")
  , ("author-email", "{{author-email}}")
  , ("author-copyright", "{{copyright}}")
  , ("current-year", "{{current-year}}")
  ]

-- | Replace all occurences of 'replaces'
replaceAll :: Text -> Text
replaceAll origText = F.foldl' go origText replaces
  where
    go t (from, to) = T.replace from to t

-- | Replace all occurences of 'replaces' and append contents to template
writeTemplate :: Handle -> FilePath -> IO ()
writeTemplate h file = do
  cnt <- T.readFile file
  let cnt' = replaceAll cnt
      file' = replaceAll (T.pack file)
      fileHeader = "{-# START_FILE " <> file' <> " #-}"
  T.hPutStrLn h fileHeader
  T.hPutStrLn h cnt'

main :: IO ()
main = do
  --[outputPath] <- getArgs
  let outputPath = "gore-and-ash-sample.hsfiles"
  withFile outputPath WriteMode $ \h -> mapM_ (writeTemplate h) files
