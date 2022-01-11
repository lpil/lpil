module Handler.Quotes (handle) where

import Prelude (pure, ($), (<$>))
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(Tuple), snd)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Foldable (find)
import Data.Context (Context)

-- | Respond to quotes from 2001: A Space Odyssey.
-- |
handle :: forall e. Context -> Aff e (Maybe String)
handle { text } =
  pure $
  snd <$>
  find matching quotes

    where
      matching (Tuple regex _) =
        test regex text


quotes :: Array (Tuple Regex String)
quotes =
  prepareRegex <$>
    [ Tuple "Do you read me"
            "Affirmative, Dave. I read you."
    , Tuple "Open the pod bay doors"
            "I'm sorry, Dave. I'm afraid I can't do that."
    ]


prepareRegex :: Tuple String String -> Tuple Regex String
prepareRegex (Tuple pattern string) =
  Tuple (unsafeRegex pattern ignoreCase) string
