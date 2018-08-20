{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module CIO.Types where

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Default
import Data.IORef
import IHaskell.Display as IHaskell
import IHaskell.Types as Types
import System.IO.Unsafe

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Database.LevelDB as DB

newtype CIO a = CIO { unCIO :: ReaderT DB.DB (ResourceT IO) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader DB.DB, MonadIO)

-- | The default cache file (@requests.cache@) used if none was set explicitly
-- with 'setCacheFile'.
defaultCacheFile :: FilePath
defaultCacheFile = "requests.cache"

-- | The global filepath used for the cache, see 'setCacheFile' and
-- 'getCacheFile' for setters accessors.
cacheFile :: IORef FilePath
cacheFile = unsafePerformIO (newIORef defaultCacheFile)
{-# NOINLINE cacheFile #-}

-- | Sets the global filepath used for the cache.
setCacheFile :: FilePath -> IO ()
setCacheFile = writeIORef cacheFile

-- | Reads the global filepath used for the cache.
getCacheFile :: IO FilePath
getCacheFile = readIORef cacheFile

-- | Runs a cached IO ('CIO') operation using the global cache file.
runCIO :: CIO a -> IO a
runCIO cio = do
    cf <- liftIO $ getCacheFile
    runCIOWith cf cio

-- | Runs a cached IO ('CIO') operation using the provided cache file.
runCIOWith :: FilePath -> CIO a -> IO a
runCIOWith cf (CIO io) = runResourceT $ do
    db <- DB.open cf def {DB.createIfMissing = True}
    runReaderT io db

-------------------------------------------------------------------------------
-- Some instances for a smooth experience with IHaskell
-------------------------------------------------------------------------------

instance
      {-# OVERLAPPING #-}
      Show a
      => IHaskell.IHaskellDisplay (CIO [a]) where
    display cio = do
        res <- runCIO cio
        pure $ IHaskell.ManyDisplay $ (\x -> IHaskell.Display [IHaskell.plain $ show x]) <$> res

instance
      {-# OVERLAPPING #-}
      Show a
      => IHaskell.IHaskellDisplay (CIO (Set.HashSet a)) where
    display = display . fmap Set.toList

instance
      {-# OVERLAPPING #-}
      (Show k, Show a)
      => IHaskell.IHaskellDisplay (CIO (Map.HashMap k a)) where
    display = display . fmap Map.toList

instance Show a => IHaskell.IHaskellDisplay (CIO a) where
    display cio = do
        res <- runCIO cio
        pure $ IHaskell.Display [IHaskell.plain $ show res]
