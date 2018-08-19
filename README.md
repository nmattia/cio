

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
    liftIO $ print (diffUTCTime stop start)
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


    (1.087115629s,"Nicolas Mattia")



```haskell
timeIt $ getWith (param "dummy" .~ [uuid] $ defaults) "https://api.github.com/users/nmattia" <&>
    (^.responseBody.key "name"._String)
```


    (0.000193292s,"Nicolas Mattia")



```haskell
import Data.Conduit
import Data.Conduit.Combinators as C
```


```haskell
sourceToList $ 
    getAllWith 
        (param "q" .~ ["lang:haskell"] $ defaults)
        "https://api.github.com/search/repositories"
    .| awaitForever (C.yieldMany . (
        ^..responseBody
        .key "items"
        .values
        .key "name"
        ._String))
    .| C.take 5
```


    "haskell-lang"



    "HaskellStudy"



    "haskell-notes"



    "i-lang"



    "impire-lang"

