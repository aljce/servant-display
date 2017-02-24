{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Display where

import Data.Kind (type Type)
import Data.Proxy (Proxy(..))
import Control.Applicative (liftA2)
import Control.Monad ((<=<))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits (KnownSymbol,symbolVal)
import Servant.API (MimeUnrender(..),ReflectMethod(..),JSON,Verb,(:>),(:<|>)(..))
import Data.Aeson (FromJSON)
import Reflex.Class (Reflex(..),MonadSample(..),FunctorMaybe(..))
import Reflex.Dom (MonadWidget(..),dyn,Workflow(..),workflowView)

import URI.ByteString (URIRef,Absolute)
import Servant.Common.Req (reqSuccess,Req(..),SupportsServantDisplay,performRequestCT
                          ,prependToPathParts,defReq)
import Servant.Common.Uri (Uri(..),performAjax,uriState,fromURIRef)

import Control.Monad.IO.Class

data Env t m = Env
  { envWidgets :: Event t ()
  , envBaseUrl :: Dynamic t (URIRef Absolute) }

data Display api t m = Display
  { displayRouter :: Uri   -> Either T.Text (Workflow t m ())
  , displayClient :: Req t -> Client api t m }

class HasDisplay t m api where
  type Server api t m :: Type
  type Client api t m :: Type
  display :: MonadWidget t m => Proxy api -> Env t m -> Server api t m -> Display api t m

serve :: (MonadWidget t m, HasDisplay t m api) =>
  Proxy api -> Dynamic t (URIRef Absolute) -> (Client api t m -> Server api t m) -> m (Event t ())
serve api baseUrl makeCbs = do
  liftIO (putStrLn "before uriState")
  -- TODO: Fix this fromRight
  uri <- fmap ((\(Right a) -> a) . fromURIRef) <$> uriState
  liftIO (putStrLn "before display")
  let (Display server client) = display api (Env undefined baseUrl) (makeCbs (client defReq))
  liftIO (putStrLn "before workflow")
  updates <- coincidence <$> dyn (fmap (either (error . T.unpack) workflowView . server) uri)
  return updates

instance (HasDisplay t m a, HasDisplay t m b) => HasDisplay t m (a :<|> b) where
  type Server (a :<|> b) t m = Server a t m :<|> Server b t m
  type Client (a :<|> b) t m = Client a t m :<|> Client b t m
  display Proxy env ~(a :<|> b) = Display choose (\req -> clientA req :<|> clientB req)
    where choose uri = case serverA uri of
            Left err -> case serverB uri of
              Left _  -> Left err
              Right a -> Right a
            Right a -> Right a
          Display serverA clientA = display (Proxy @a) env a
          Display serverB clientB = display (Proxy @b) env b

unitWorkFlow :: (Reflex t, Monad m) => Workflow t m ()
unitWorkFlow = Workflow (return ((),never))

instance (ReflectMethod method, FromJSON a, SupportsServantDisplay t m) =>
  HasDisplay t m (Verb method status '[JSON] a) where
  type Server (Verb method status '[JSON] a) t m = a -> Workflow t m ()
  type Client (Verb method status '[JSON] a) t m = Event t () -> Workflow t m ()
  display Proxy (Env widgets baseUrl) cb = Display server client
    where server req = Right . Workflow $ do
            liftIO $ putStrLn "Inside verb"
            url <- sample (current baseUrl)
            res <- performAjax url req
            case res of
              Right a -> unWorkflow (cb a)
              Left _  -> unWorkflow unitWorkFlow
          client req e = Workflow $ do
            let method = T.decodeUtf8 (reflectMethod (Proxy @method))
            reqs <- performRequestCT (Proxy @JSON) (req { reqMethod = method }) baseUrl e
            -- Does unit workflow here make sense?
            return ((),fmap (maybe unitWorkFlow cb . reqSuccess) reqs)


errMsg :: T.Text
errMsg = T.pack "error 404 not found"

instance (KnownSymbol path, HasDisplay t m api) => HasDisplay t m (path :> api) where
  type Server (path :> api) t m = Server api t m
  type Client (path :> api) t m = Client api t m
  display Proxy env cb = Display server' client'
    where server' uri = case uriPathPieces uri of
            [] -> Left errMsg
            (piece:rest) -> case piece == path of
              False -> Left errMsg
              True  -> server (uri { uriPathPieces = rest })
          client' req = client (pure (Right path) `prependToPathParts` req)
          path = T.pack (symbolVal (Proxy @path))
          Display server client = display (Proxy @api) env cb



-- instance (ToHttpApiData a, FromHttpApiData a, HasDisplay t m api) => HasDisplay t m (Capture capture a :> api) where
--   type Server (Capture capture a :> api) t m = Server t m api
--   type Client t m (Capture capture a :> api) = Behavior t a -> Client t m api
--   display Proxy (Env widget)
