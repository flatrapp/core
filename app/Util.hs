module Util where

import           Control.Monad
import           Data.Text     (Text, pack)
import           System.Random

randomText :: Int-> IO Text
randomText length = liftM (pack . take length . randomRs ('a','z')) newStdGen
