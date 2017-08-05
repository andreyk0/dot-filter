{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Args (
  Args(..)
, runWithArgs
) where


import           Control.Monad
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Development.GitRev
import           Options.Applicative
import           System.Exit


data Args = Args { argsVersion :: !Bool
                 , argsKeywords :: ![Text]
                 } deriving (Show)


parseArgs :: Parser Args
parseArgs = Args
     <$> switch
         ( long "version"
        <> short 'V'
        <> help "Print version and exit.")
     <*> some (T.pack <$> argument str (metavar "keyword"))


runWithArgs :: (Args -> IO ())
           -> IO ()
runWithArgs rwa = execParser opts >>= printVersion >>= rwa
  where
    opts = info (helper <*> parseArgs)
      ( fullDesc
     <> header "Tiny hack to filter simple dot graphs to find nodes/edges matching a keyword."
     <> progDesc "E.g. $ cat foo.dot | dot-filter keyword > bar.dot")

    printVersion args@Args{..} = do
      when argsVersion $ die $ "Version: " <> $(gitBranch) <> "@" <> $(gitHash)
      return args
