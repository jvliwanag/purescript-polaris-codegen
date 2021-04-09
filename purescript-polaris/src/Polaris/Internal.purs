module Polaris.Internal
       ( PropsWithChildren'
       , PropsWithChildren
       , elem
       , elemWithChildren
       ) where

import Prelude

import Data.Symbol (SProxy(..))
import Prim.Row as Row
import React.Basic.Hooks (JSX, ReactChildren, ReactComponent, element, reactChildrenFromArray)
import Record as Record
import Untagged.Castable (class Castable, cast)

type PropsWithChildren' a p = {children :: ReactChildren a| p}
type PropsWithChildren p = PropsWithChildren' JSX p

elem :: forall r props. Castable r {|props} => ReactComponent {|props} -> r -> JSX
elem rc = element rc <<< cast

elemWithChildren ::
  forall r baseProps a.
  Row.Lacks "children" baseProps =>
  Castable r {|baseProps} =>
  ReactComponent (PropsWithChildren' a baseProps) ->
  r ->
  Array a ->
  JSX
elemWithChildren rc r children = element rc props
  where
    baseProps = cast r :: {|baseProps}
    props = Record.insert (SProxy :: _ "children")
            (reactChildrenFromArray children)
            baseProps
