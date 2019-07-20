module Hal where

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Context (Context)
import Handler.Quotes as Quotes

handle :: forall e. Context -> Aff e (Maybe String)
handle =
  Quotes.handle
