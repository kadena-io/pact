module Utils
( testMgr
) where

import qualified Network.HTTP.Client as HTTP

import System.IO.Unsafe

-- | Use a single global Manager throughout all tests
--
testMgr :: HTTP.Manager
testMgr = unsafePerformIO $ HTTP.newManager HTTP.defaultManagerSettings
{-# NOINLINE testMgr #-}

