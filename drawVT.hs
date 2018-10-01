--Created: April 03, 2018
--by David Osorio

{-
 * REFERENCES:
 * https://www.schoolofhaskell.com/user/davorak/code-snippets/zipper-tree-examples
 * https://hackage.haskell.org/package/pretty-tree-0.1.0.0/docs/Data-Tree-Pretty.html
 * https://hackage.haskell.org/package/pretty-tree
 * https://hackage.haskell.org/package/boxes
 * https://hackage.haskell.org/package/base
-}


import Data.Tree
import Data.Tree.Pretty (drawVerticalTree)
--import Data.Tree.Pretty
--import Data.Tree.Lens


tree :: Tree String
tree = Node "hello" [ Node "foo" []
                    , Node "bars" [ Node "oi!" []
                                , Node "baz" [ Node "a" [ Node "b" []
                                                        , Node "c" []]
                                            , Node "d" [ Node "e" []]]]
                    , Node "foobar" []]