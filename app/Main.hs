{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import System.Process
import System.IO
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM as STM
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, FileInfo(..))
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (readInt, unpack)
import Control.Monad (forever)
import System.Directory (createDirectoryIfMissing, renamePath)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Options.Applicative as O
import Data.Char8 (isAlphaNum)
import Control.Exception (SomeException, try)

type Queue = STM.TVar [FilePath]
type ProgressSet = STM.TVar (S.Set FilePath)

bootstrapPage :: H.Html -> H.Html -> H.Html
bootstrapPage title inner = H.docTypeHtml $ do
  H.head $ do
    H.title title
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.type_ "text/css"
      H.! HA.href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
    H.script
      H.! HA.src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"
      H.! HA.type_ "text/javascript"
      $   ""
  H.body $
    H.div
      H.! HA.class_ "container"
      $ do
        H.h1 title
        inner

uploadForm :: T.Text -> BL.ByteString
uploadForm bp = H.renderHtml $ bootstrapPage "Upload video" $ do
  H.form
    H.! HA.action (H.textValue $ bp `T.append` "/upload")
    H.! HA.method "post"
    H.! HA.enctype "multipart/form-data" $ do
      H.label "Szene:"
      H.select
        H.! HA.name "scene"
        H.! HA.class_ "form-select" $ do
          H.option
            H.! HA.value "1"
            $ "Gruppe 1"
          H.option
            H.! HA.value "2"
            $ "Gruppe 2"
          H.option
            H.! HA.value "3"
            $ "Gruppe 3"
          H.option
            H.! HA.value "4"
            $ "Gruppe 4"
          H.option
            H.! HA.value "5"
            $ "Gruppe 5"
      H.br
      H.label "Take:"
      H.input
        H.! HA.type_ "number"
        H.! HA.name "take"
        H.! HA.min "1"
        H.! HA.max "99"
        H.! HA.class_ "form-control"
      H.br
      H.label "Kamera:"
      H.input
        H.! HA.type_ "text"
        H.! HA.name "camera"
        H.! HA.class_ "form-control"
      H.br
      H.label "Videodatei:"
      H.input
        H.! HA.type_ "file"
        H.! HA.name "file"
        H.! HA.accept "video/*"
        H.! HA.class_ "form-control"
      H.br
      H.input
        H.! HA.type_ "submit"
        H.! HA.value "Upload"
        H.! HA.class_ "btn btn-primary"

uploadResponse :: T.Text -> BL.ByteString
uploadResponse bp = H.renderHtml $ bootstrapPage "Upload successful" $ do
  H.p "Datei erfolgreich hochgeladen."
  H.p "Das Video wird in Kürze konvertiert. Das kann ein bisschen dauern."
  H.p $ do
    "Es taucht dann unter "
    H.a "/converted/" H.! HA.href (H.textValue "/converted/")
    " auf."
  H.p $ do
    "Die Warteschlange findest du unter "
    H.a "hier" H.! HA.href (H.textValue $ bp `T.append` "/queue")
    "."
  H.p $ do
    H.a "Weiteres Video hochladen" H.! HA.href (H.textValue bp)

queueResponse :: T.Text -> [FilePath] -> [FilePath] -> BL.ByteString
queueResponse bp qs ps = H.renderHtml $ bootstrapPage "Queue" $ do
  H.h2 "In Verarbeitung"
  mapM_ (H.p . H.toHtml) ps
  H.h2 "In der Warteschlange"
  mapM_ (H.p . H.toHtml) qs
  H.h2 "Weiteres Video hochladen"
  H.p $ do
    H.a "Weiteres Video hochladen" H.! HA.href (H.textValue bp)

-- Main application
app :: FilePath -> [T.Text] -> Queue -> ProgressSet -> Application
app fileBase' basePath pending inProgress req respond
  | p == basePath               = respond $ responseLBS status200 [("Content-Type", "text/html")]
                                          $ uploadForm bp
  | p == basePath ++ ["upload"] = handleUpload fileBase' bp pending req respond
  | p == basePath ++ ["queue"]  = handleQueue bp pending inProgress req respond
  | otherwise                   = respond $ responseLBS status400 [("Content-Type", "text/plain")]
                                            "Not Found"
  where p = pathInfo req
        bp = T.concat $ map ("/" `T.append`) basePath

-- Handle file upload and conversion
handleUpload :: FilePath -> T.Text -> Queue -> Application
handleUpload fileBase' bp q req respond = do
  (params, files) <- parseRequestBody lbsBackEnd req
  let pScene = max 0 $ maybe 0 fst (lookup "scene" params >>= readInt)
  let pTake = max 0 $ maybe 0 fst (lookup "take" params >>= readInt)
  let pCamera = sanitizeCam $ fromMaybe "Unknown" (lookup "camera" params)
  let fileName = digits 3 pScene <> "_" <> digits 2 pTake <> "_" <> pCamera
                 where digits n x = B8.pack $ replicate (n - length (show x)) '0' ++ show x

  case lookup "file" files of
    Nothing -> respond $ responseLBS status400 [("Content-Type", "text/plain")] "No file uploaded"
    Just fileInfo -> do
      let inputFile = fileContent fileInfo
      withBinaryFile (fileBase' <> "/uploads/" <> unpack fileName) WriteMode $ \uploadHandle -> do
        BL.hPutStr uploadHandle inputFile
      STM.atomically $ STM.modifyTVar q (++ [unpack fileName])
      respond $ responseLBS status200 [("Content-Type", "text/html")] $ uploadResponse bp

sanitizeCam :: B8.ByteString -> B8.ByteString
sanitizeCam = B8.map (\c -> if isAlphaNum c then c else '_')

handleQueue :: T.Text -> Queue -> ProgressSet -> Application
handleQueue bp pending inProgress _req respond = do
  (qs, ps) <- STM.atomically $ do
    qs <- STM.readTVar pending
    ps <- S.toList <$> STM.readTVar inProgress
    pure (qs, ps)
  respond $ responseLBS status200 [("Content-Type", "text/html")] $ queueResponse bp qs ps

worker :: FilePath -> FilePath -> Queue -> ProgressSet ->  IO ()
worker fileBase' ffmpegPath' pending inProgress = forever $ do
  f <- STM.atomically $ do
    qs <- STM.readTVar pending
    case qs of
      [] -> STM.retry
      (f:fs) -> do
        STM.writeTVar pending fs
        STM.modifyTVar inProgress (S.insert f)
        pure f

  putStrLn $ "Converting " ++ f
  let inputFileName = fileBase' ++ "/uploads/" ++ f
  let outputFileName = fileBase' ++ "/tmp/" ++ f ++ ".mov"
  let ffmpegArgs = ["-i", inputFileName, "-c:v", "dnxhd", "-vf", "scale=1920:1080", "-b:v", "120M", "-c:a", "pcm_s16le", "-f", "mov", "-y", outputFileName]
  _ <- try @SomeException $ do
    (_, _, _, ph) <- createProcess (proc ffmpegPath' ffmpegArgs)
    _ <- waitForProcess ph
    renamePath (fileBase' ++ "/tmp/" ++ f ++ ".mov") (fileBase' ++ "/converted/" ++ f ++ ".mov")
  putStrLn $ "Done converting " ++ f

  STM.atomically $ STM.modifyTVar inProgress (S.delete f)

data Config = Config
  { basePath :: !T.Text
  , fileBase :: !FilePath
  , ffmpegPath :: !FilePath
  , port :: !Int
  }

configParser :: O.Parser Config
configParser = Config
  <$> O.option O.str
      ( O.long "base-path"
     <> O.metavar "PATH"
     <> O.help "Base path for the application"
     <> O.value "/"
     <> O.showDefault
      )
  <*> O.option O.str
      ( O.long "file-base"
     <> O.metavar "PATH"
     <> O.help "Base path for uploaded files"
     <> O.value "./"
     <> O.showDefault
      )
  <*> O.option O.str
      ( O.long "ffmpeg-path"
     <> O.metavar "PATH"
     <> O.help "Path to the ffmpeg executable"
     <> O.value "ffmpeg"
     <> O.showDefault
      )
  <*> O.option O.auto
      ( O.long "port"
     <> O.metavar "PORT"
     <> O.help "Port to run the server on"
     <> O.value 3000
     <> O.showDefault
      )
  
main :: IO ()
main = do
  config <- O.execParser $ O.info (configParser O.<**> O.helper)
    ( O.fullDesc
    <> O.progDesc "Simple video conversion server"
    <> O.header "video-converter - a simple video conversion server"
    )

  createDirectoryIfMissing True (fileBase config ++ "/uploads")
  createDirectoryIfMissing True (fileBase config ++ "/tmp")
  createDirectoryIfMissing True (fileBase config ++ "/converted")
  let basePath' = filter (not . T.null) $ T.splitOn "/" (basePath config)
  queue <- STM.newTVarIO []
  inProgress <- STM.newTVarIO S.empty
  _ <- forkIO $ worker (fileBase config) (ffmpegPath config) queue inProgress
  run (port config) (app (fileBase config) basePath' queue inProgress)
