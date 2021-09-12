{-# LANGUAGE OverloadedStrings #-}

module Backend
    ( startServer
    ) where

import Web.Scotty

import Data.Monoid (mconcat)

homeEndpoint :: ActionM ()
homeEndpoint = do
  setHeader "Content-Type" "text/html"
  file "./src/assets/index.html"

imagesEndpoint :: ActionM ()
imagesEndpoint = do
  fileName <- param "fileName"
  file ("./src/assets/images/" ++ fileName)

endpoints :: ScottyM ()
endpoints = do
  get "/" homeEndpoint
  get "/assets/index.js" $ file "./src/assets/index.js"
  get "/assets/images/:fileName" imagesEndpoint

startServer :: IO ()
startServer = scotty 3000 $
  endpoints
