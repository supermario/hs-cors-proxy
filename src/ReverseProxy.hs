{-# LANGUAGE OverloadedStrings #-}

module ReverseProxy where

import Control.Exception (throw)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import Text.Read (readMaybe)

import Network.HTTP.ReverseProxy
import Data.Function ((&))

{- Rough implementation of a reverse proxy, mirroring some of the techniques from cors-anywhere:

https://github.com/Rob--W/cors-anywhere/blob/master/lib/cors-anywhere.js

-}


main :: IO ()
main = do
  let port = 8222
  putStrLn $ "Starting proxy on " <> show port <> "..."
  startReverseProxy port


startReverseProxy :: Int -> IO ()
startReverseProxy proxyPort = do
  manager <- newManager tlsManagerSettings

  let
    settings =
      defaultSettings
        & setPort proxyPort
        & setHost "0.0.0.0"

  runSettings settings $
    waiProxyTo
      (\request ->
        case requestHeaderHost request of
          Just _host -> do
            -- debug "------"
            -- debug_ $ show request

            res <-
              case requestMethod request of
                "OPTIONS" ->
                  pure $ WPRResponse $ responseLBS status200 ([] & addCors request) ""

                _ -> do
                  case pathInfo request of
                    "http:":"":hostname:rest -> do
                      let
                        (hostnameClean, targetPort) =
                          case Text.splitOn ":" hostname of
                            hostnameClean_:port_:[] ->
                              (hostnameClean_, port_ & Text.unpack & readMaybe & withDefault 80)

                            _ ->
                              (hostname, 80)

                      pure $ WPRModifiedRequest
                        (modReq rest hostnameClean request)
                        (ProxyDest (T.encodeUtf8 hostnameClean) targetPort)

                    "https:":"":hostname:rest ->
                      pure $ WPRModifiedRequestSecure
                        (modReq rest hostname request)
                        (ProxyDest (T.encodeUtf8 hostname) 443)

                    _ ->
                      error "bad path"

            -- debug_ $ "\n➡️  proxying with: " ++ show res
            pure res

          Nothing ->
            error "Request header host missing"
      )
      (\exception -> throw exception )
      manager


{- Adjust the request to remove the effects of the http://localhost:xxxx URL prefix -}
modReq :: [Text] -> Text -> Network.Wai.Request -> Network.Wai.Request
modReq newPathInfo hostname request =
  request
    { pathInfo = newPathInfo
    , rawPathInfo = newPathInfo & Text.intercalate "/" & Text.append "/" & T.encodeUtf8
    , requestHeaderHost = Just $ T.encodeUtf8 hostname
    , Network.Wai.requestHeaders =
        request
          & Network.Wai.requestHeaders
          & fmap (\(header, contents) ->
            if header == hHost
              then (header, T.encodeUtf8 hostname)
              else (header, contents)
          )
    }


withDefault :: a -> Maybe a -> a
withDefault default_ m =
  case m of
    Just v -> v
    Nothing -> default_
