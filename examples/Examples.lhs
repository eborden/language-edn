# Language EDN

This is a simple representation of the EDN language in Haskell. EDN is a
derivitive of Clojure's information model. More information can be found here:
<https://github.com/edn-format/edn>.

```haskell
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens (preview, set)
import Language.EDN

janeDoe :: EDN
janeDoe = mapOf
  [ #name, "Jane Doe"
  , #age, int 34
  , #favorite_books, setOf ["a tale of two cities", "the kite runner"]
  , #some_tagged_value, #my_tag (float 72.234)
  , #a_few_of_my_favorite_things, [#rain_drops_on_roses, #whiskers_on_kittens]
  , #address, mapOf
    [ #state, "ny"
    , #zip, int 11225
    ]
  ]

main :: IO ()
main = do
  print janeDoe
  print (preview (key #name . _String) janeDoe)
  print (preview (key #address . key #zip . _Integer) janeDoe)
  print (set (key #address . key #zip . _Integer) 11237 janeDoe)
  print (set (keyAt #height) (Just (float 56.4)) janeDoe)
```
