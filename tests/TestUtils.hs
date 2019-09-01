{-# LANGUAGE CPP #-}

module TestUtils where

import Servant.Client

#if !MIN_VERSION_servant_client(0,16,0)
type ClientError = ServantError
#endif
