{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Servant.API
import qualified URI.ByteString as U
import Reflex.Dom hiding (display)
import qualified Reflex.Dom as R
import Control.Monad.IO.Class

import Servant.Common.Uri (uriState)
import Servant.Display

type API = "item" :> "all" :> Get '[JSON] [Int]
      :<|> "item" :> "one" :> Get '[JSON] Int
      :<|> Get '[JSON] ()

runGUI :: forall t m. MonadWidget t m => m ()
runGUI = do
  (Right uri) <- return (U.parseURI U.laxURIParserOptions "http://localhost:8080")
  liftIO (putStrLn "Before serve")
  -- uriState >>= R.display
  el "div" $ text "Reflex single page app!"
  changes <- serve (Proxy @API) (pure uri) $ \(jumpOne :<|> jumpAll :<|> jumpHome) ->
    displayAll jumpOne :<|> displayOne jumpAll :<|> index jumpOne
  -- (count changes :: m (Dynamic t Int)) >>= R.display
  return ()
  where displayAll :: (Event t () -> Workflow t m ()) -> [Int] -> Workflow t m ()
        displayAll jump list = Workflow $ do
          liftIO (putStrLn "Display all")
          mapM_ (text . T.pack . show) list
          b <- button "Jump to one item!"
          unWorkflow (jump b)
        displayOne :: (Event t () -> Workflow t m ()) -> Int -> Workflow t m ()
        displayOne jump item = Workflow $ do
          liftIO (putStrLn "Display one")
          (text . T.pack . show) item
          b <- button "Jump to all items!"
          unWorkflow (jump b)
        index :: (Event t () -> Workflow t m ()) -> () -> Workflow t m ()
        index jump () = Workflow $ do
          b <- button "Jump to one item!"
          unWorkflow (jump b)

main :: IO ()
main = mainWidget runGUI
