{-# LANGUAGE CPP #-}

module TestUtils where

#if !MIN_VERSION_servant_client(0,16,0)
import Servant.Client

type ClientError = ServantError
#endif
