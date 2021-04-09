module Polaris.Types
       ( Action
       , action
       ) where

import Prelude

import Effect (Effect)
import Untagged.Castable (class Castable, cast)
import Untagged.Union (UndefinedOr)

-- https://github.com/Shopify/polaris-react/blob/69ec09dbe49662cd7ef95670803073987f1d1ce3/src/types.ts#L87
type Action =
  { id :: UndefinedOr String
  , content :: UndefinedOr String
  , accessibilityLabel :: UndefinedOr String
  , url :: UndefinedOr String
  , external :: UndefinedOr Boolean
  , onAction :: UndefinedOr (Effect Unit)
  , onMouseEnter :: UndefinedOr (Effect Unit)
  , onTouchStart :: UndefinedOr (Effect Unit)
  }

action :: forall r. Castable r Action => r -> Action
action = cast
