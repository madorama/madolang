module Madlib.Pretty where

import qualified Prettyprinter             as PP
import qualified Prettyprinter.Render.Text as PRT

import           Madlib.Operator

putDoc :: PP.Pretty a => a -> IO ()
putDoc =
  PRT.putDoc . PP.pretty

add :: PP.Doc ann -> PP.Doc ann -> PP.Doc ann
add x y =
  x
  |> (PP.<>) y

addSpace :: PP.Doc ann -> PP.Doc ann -> PP.Doc ann
addSpace x y =
  x
  |> (PP.<+>) y

addPretty :: PP.Pretty a => a -> PP.Doc ann -> PP.Doc ann
addPretty a x =
  x
  |> add (PP.pretty a)

addNest :: Int -> PP.Doc ann -> PP.Doc ann -> PP.Doc ann
addNest n x y =
  y
  |> add (PP.nest n x)

addList :: PP.Pretty a => [a] -> PP.Doc ann -> PP.Doc ann
addList as x =
  x
  |> add (PP.prettyList as)

join :: PP.Doc ann -> [PP.Doc ann] -> PP.Doc ann
join sep docs =
  PP.punctuate sep docs
  |> PP.hcat

ifMaybe :: (a -> PP.Doc ann) -> Maybe a -> PP.Doc ann
ifMaybe f m =
  m
  |> maybe PP.emptyDoc f

blockWithEmpty :: PP.Doc ann -> ([a] -> PP.Doc ann) -> [a] -> PP.Doc ann
blockWithEmpty prefix f =
  \case
    [] ->
      prefix
      |> addSpace (PP.braces PP.emptyDoc)

    xs ->
      prefix
      |> addSpace
        ( PP.braces
            ( PP.nest 4
                ( PP.line
                  |> add (f xs)
                )
              |> add PP.line
            )
        )

block :: PP.Doc ann -> ([a] -> PP.Doc ann) -> [a] -> PP.Doc ann
block prefix f =
  \case
    [] ->
      PP.emptyDoc

    xs ->
      blockWithEmpty prefix f xs

