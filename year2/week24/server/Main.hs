{-# LANGUAGE OverloadedStrings #-}

import qualified User
import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy.IO as T

htmlFile f = do
    contents <- liftIO (T.readFile f)
    html contents

main :: IO ()
main = scotty 3000 $ do
    get "/login" $ htmlFile "login.html"

    post "/auth" $ do
        username <- param "username"
        password <- param "password"
        ret      <- liftIO (User.isValid username password)
        if null ret
            then text "invalid login"
            else text "valid login"
