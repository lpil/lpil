module Main
  ( app
  , requestHandler
  ) where

import Prelude hiding (apply)
import Control.Monad.Eff.Exception (Error, message)
import Node.Express.App (App, useOnError, use, get)
import Node.Express.Handler (Handler)
import Node.Express.Response (sendJson, setStatus)


errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 400
  sendJson { error: message err }


notFoundHandler :: forall e. Handler e
notFoundHandler = do
  setStatus 404
  sendJson { error: "resource not found" }


indexHandler :: forall e. Handler e
indexHandler = do
  sendJson { status: "ok" }


app :: forall e. App e
app = do
  get "/" indexHandler
  use notFoundHandler
  useOnError errorHandler


-- Not really Unit...
foreign import makeRequestHandler :: forall e. App e -> Unit


{-| A request handler that allows the application to be used
within Google Cloud functions.
-}
requestHandler :: Unit
requestHandler =
  makeRequestHandler app
