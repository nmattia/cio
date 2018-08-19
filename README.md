

```haskell
{-# LANGUAGE OverloadedStrings #-}

import CIO
import Data.Aeson.Lens
import Control.Lens
```


```haskell
get "https://api.github.com/users/nmattia" <&>
    (^.responseBody.key "name"._String)
```


    "Nicolas Mattia"



```haskell
import Control.Monad.IO.Class
import Data.Time

timeIt :: CIO a -> CIO (NominalDiffTime, a)
timeIt act = do
    start <- liftIO $ getCurrentTime
    res <- act
    stop <- liftIO $ getCurrentTime
    pure (diffUTCTime stop start, res)
```


```haskell
import Data.UUID (toText)
import System.Random (randomIO)

uuid <- toText <$> randomIO
```


```haskell
timeIt $ getWith (param "dummy" .~ [uuid] $ defaults) "https://api.github.com/users/nmattia" <&>
    (^.responseBody.key "name"._String)
```


    (1.370929802s,"Nicolas Mattia")



```haskell
timeIt $ getWith (param "dummy" .~ [uuid] $ defaults) "https://api.github.com/users/nmattia" <&>
    (^.responseBody.key "name"._String)
```


    (0.00025088s,"Nicolas Mattia")



```haskell
import Data.Conduit
import Data.Conduit.Combinators as C
```


```haskell
sourceToList $ 
    getAllWith 
        (defaults 
        & param "q" .~ ["language:haskell"] 
        & param "sort" .~ ["stars"]
        & param "per_page" .~ ["5"])
        "https://api.github.com/search/repositories"
    .| awaitForever (C.yieldMany . (
        ^..responseBody
        .key "items"
        .values
        .key "name"
        ._String))
    .| C.take 15
```


    "pandoc"



    "shellcheck"



    "postgrest"



    "purescript"



    "compiler"



    "Haxl"



    "cardano-sl"



    "stack"



    "Idris-dev"



    "luna"



    "Functional-Programming"



    "Carp"



    "write-you-a-haskell"



    "ghcjs"



    "yesod"

