{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (toJSON, ToJSON, (.=), object)
import qualified Web.Scotty as S

import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM)
import Web.Scotty.Trans (ActionT)

import qualified Eval as E

extract :: E.Result -> String
extract (Just (Right s)) = s
extract (Just (Left s)) = s
extract _ = "Command not supported"

data Response = Response { text :: String }
instance ToJSON Response where
     toJSON (Response text) = object ["text" .= text]

toJson :: IO E.Result -> ActionM ()
toJson m = liftIO m >>= S.json . makeJson . extract where
    makeJson s = Response { text = s }

main = S.scotty 3000 $ do
         S.post "/slack" $ do
                       liftIO $ putStrLn $ "POST!"
                       text <- S.param "text"
                       liftIO $ putStrLn $ text
                       let cmd = E.parse text
                       toJson (E.exec cmd)
                       
                       
