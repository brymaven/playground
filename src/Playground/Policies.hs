module Playground.Policies where

import Playground.Types
import Data.Map (findWithDefault, Map)

-- Policy with Map as backing structure
lookupPolicy :: Ord s => a -> Map s a -> Policy s a
lookupPolicy d m = Policy $ \s -> findWithDefault d s m
