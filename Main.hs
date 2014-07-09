{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Text.Lazy
import Web.Scotty
import Web.Scotty.Trans (ActionT)
import Eval

channel = "slack-sandbox"
name = "Mr. Haskell"

makeUrl token channel name text = "https://slack.com/api/chat.postMessage?" ++
                                  "?token=" ++ token ++
                                  "&channel=%23" ++ channel ++
                                  "&text=" ++ text ++
                                  "&username=" ++ name

makeHtml :: IO Result -> ActionM ()
makeHtml m = liftIO m >>= html . pack. write where
    write (Just (Right s)) = s
    write (Just (Left s)) = s
    write _ = "Command not supported"

main = scotty 3000 $ do
         get "/slack" $ do
                       text <- param "text"
                       let cmd = parse text
                       makeHtml $ exec cmd
