{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Servant.Common.Uri where

import Control.Applicative (liftA2)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified URI.ByteString as U
import Data.Monoid ((<>))
import Data.Bifunctor (Bifunctor(..))

import Data.Aeson (FromJSON)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Monad.IO.Class (MonadIO(..))
import Reflex.Dom (HasWebView,def)
import Reflex.Dom.Xhr (newXMLHttpRequest,xhrRequest,decodeXhrResponse)

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Reflex.Dom                hiding (EventName, Window)
import qualified GHCJS.DOM                 as DOM
import           GHCJS.DOM.Document        (getDefaultView)
import           GHCJS.DOM.EventM          (on)
import           GHCJS.DOM.History         (History, back, forward, pushState)
import           GHCJS.DOM.Location        (toString,Location)
import           GHCJS.DOM.Window          (Window, getHistory,
                                            getLocation, popState)
import           GHCJS.Marshal.Pure

data Uri = Uri
  { uriPathPieces :: [T.Text]
  , uriQuery      :: [(T.Text,T.Text)] }

fromURIRef :: U.URIRef a -> Either T.Text Uri
fromURIRef (U.URI _ _ path querys _) = buildUri path querys
fromURIRef (U.RelativeRef _ path querys _) = buildUri path querys

buildUri :: BS.ByteString -> U.Query -> Either T.Text Uri
buildUri path querys = first (T.pack . show) (liftA2 Uri (buildPath path) (buildQuerys querys))
  where buildPath = fmap (T.splitOn "/") . T.decodeUtf8'
        buildQuerys = traverse (\(t1,t2) -> liftA2 (,) (T.decodeUtf8' t1) (T.decodeUtf8' t2)) . U.queryPairs

encodeURI :: U.URIRef a -> Uri -> T.Text
encodeURI start (Uri paths querys) =
  T.decodeUtf8 (U.serializeURIRef' start) <> T.intercalate "/" paths <> "?" <>
  T.intercalate "&" (fmap (\(t1,t2) -> t1 <> "=" <> t2) querys)

performAjax :: (MonadIO m, HasWebView m, FromJSON a) => U.URIRef U.Absolute -> Uri -> m (Either String a)
performAjax start rest = do
  var <- liftIO newEmptyMVar
  liftIO (putStrLn "Performing ajax")
  _ <- newXMLHttpRequest (xhrRequest "GET" (encodeURI start rest) headers) (\xhrRes -> putMVar var xhrRes)
  res <- liftIO (fmap (note "Invalid json from server." . decodeXhrResponse) (takeMVar var))
  case res of
    Left err -> liftIO (putStrLn err) >> return (Left err)
    Right a -> return (Right a)
  where note e Nothing  = Left e
        note _ (Just x) = Right x
        headers = def { _xhrRequestConfig_headers = "Content-type" =: "application/json" }

uriState :: (MonadWidget t m) => m (Dynamic t (U.URIRef U.Absolute))
uriState = do
  start <- getURI
  future <- getPopState
  holdDyn start future

-- | Get the DOM window object.
askDomWindow :: (HasWebView m, MonadIO m) => m Window
askDomWindow = do
  wv <- askWebView
  Just doc <- liftIO . DOM.webViewGetDomDocument $ unWebViewSingleton wv
  Just window <- liftIO $ getDefaultView doc
  return window

-------------------------------------------------------------------------------
getPopState :: (MonadWidget t m) => m (Event t (U.URIRef U.Absolute))
getPopState = do
  window <- askDomWindow
  wrapDomEventMaybe window (`on` popState) $ liftIO $ do
    Just loc <- getLocation window
    locStr <- toString loc
    return . hush $ U.parseURI U.laxURIParserOptions (T.encodeUtf8 locStr)
  where hush (Left _)  = Nothing
        hush (Right a) = Just a


-------------------------------------------------------------------------------
goForward :: (HasWebView m, MonadIO m) => m ()
goForward = withHistory forward


-------------------------------------------------------------------------------
goBack :: (HasWebView m, MonadIO m) => m ()
goBack = withHistory back


-------------------------------------------------------------------------------
withHistory :: (HasWebView m, MonadIO m) => (History -> IO a) -> m a
withHistory act = do
  Just h <- liftIO . getHistory =<< askDomWindow
  liftIO $ act h


-------------------------------------------------------------------------------
-- | (Unsafely) get the 'GHCJS.DOM.Location.Location' of a window
getLoc :: (HasWebView m, MonadIO m) => m Location
getLoc = do
  Just win <- liftIO . getLocation =<< askDomWindow
  return win

-------------------------------------------------------------------------------
-- | (Unsafely) get the URL text of a window
getUrlText :: (HasWebView m, MonadIO m) => m T.Text
getUrlText = getLoc >>= liftIO . toString


-------------------------------------------------------------------------------
getURI :: (HasWebView m, MonadIO m) => m (U.URIRef U.Absolute)
getURI = do
  l <- getUrlText
  return $ either (error "No parse of window location") id .
    U.parseURI U.laxURIParserOptions $ T.encodeUtf8 l


foreign import javascript unsafe "w = window; e = new PopStateEvent('popstate',{'view':window,'bubbles':true,'cancelable':true}); w['dispatchEvent'](e);"
  dispatchEvent' :: IO ()
