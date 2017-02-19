{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import qualified Brick.AttrMap          as A
import           Brick.BChan
import qualified Brick.Main             as M
import           Brick.Types            (BrickEvent (..), CursorLocation,
                                         EventM, Next, Widget (..), attrL,
                                         emptyResult, handleEventLensed)
import           Brick.Util             (on)
import qualified Brick.Widgets.Center   as C
import           Brick.Widgets.Core     (hLimit, str, vLimit, (<+>), (<=>))
import qualified Graphics.Vty           as V
import           Lens.Micro
import           Lens.Micro.TH

import qualified EditRope               as E
import qualified Yi.Rope                as Y

import           ErrUtils               (mkPlainErrMsg)
import           FastString             (mkFastString)
import           GHC
import           GHC.Paths              (libdir)
import           Lexer
import qualified MonadUtils             as GMU
import           SrcLoc
import           StringBuffer

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import qualified Control.FoldDebounce   as Fdeb
import           System.IO

data EditorName =
  Edit1 | Edit2
  deriving (Ord, Show, Eq)

newtype St =
    St { _edit1     :: E.Editor EditorName }
makeLenses ''St

drawUI :: St -> [Widget EditorName]
drawUI st = [ui]
    where
        e1 = E.renderEditor True (st^.edit1)
        ui = C.center $ hLimit 30 (vLimit 5 e1)

appEvent :: St -> BrickEvent EditorName (E.TokenizedEvent [GHC.Located GHC.Token]) -> EventM EditorName (Next St)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc [])   -> M.halt st

        appEvt@(AppEvent (E.Tokens _)) -> handleInEditor st appEvt

        vtyEv@(VtyEvent (V.EvKey _ _)) -> handleInEditor st vtyEv

        _                              -> M.continue st

handleInEditor :: St -> BrickEvent EditorName (E.TokenizedEvent [GHC.Located GHC.Token]) -> EventM EditorName (Next St)
handleInEditor st e =
  M.continue =<< handleEventLensed st edit1 E.handleEditorEvent e

initialState :: (String -> IO ()) -> St
initialState sendSource =
    St (E.editor Edit1 "edit1" sendSource)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr, V.white `on` V.black)
     ,(A.attrName "ITinteger", V.withStyle (V.green `on` V.black) V.bold)
     ,(A.attrName "ITvarid", V.withStyle (V.blue `on` V.black) V.bold)
    ]

theApp :: M.App St (E.TokenizedEvent [GHC.Located GHC.Token]) EditorName
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return -- TODO: initial tokenization !
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
  eventChannel <- newBChan 1
  lexerChannel <- liftIO newEmptyMVar

  liftIO $ startGhc eventChannel lexerChannel

  let callback = putMVar lexerChannel
  -- trigger <- Fdeb.new Fdeb.Args { Fdeb.cb = callback, Fdeb.fold = \_ i -> i, Fdeb.init = "" }
  --                     Fdeb.def { Fdeb.delay = 100000 }
  -- let sendSource = Fdeb.send trigger
  -- let initSt = initialState sendSource
  let initSt = initialState callback
  
  st <- M.customMain (V.mkVty V.defaultConfig) (Just eventChannel) theApp initSt

  putStrLn "In input 1 you entered:\n"
  putStrLn $ Y.toString $ E.getEditContents $ st^.edit1


startGhc :: BChan (E.TokenizedEvent [Located Token]) -> MVar String -> IO ()
startGhc eventChannel lexerChannel = do
  _ <- forkIO $ runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    let lexLoc = mkRealSrcLoc (mkFastString "<interactive>") 1 1
    GMU.liftIO $ forever $ do
        src <- takeMVar lexerChannel
        let sb = stringToStringBuffer src
        let pResult = lexTokenStream sb lexLoc flags
        case pResult of

          POk _ toks -> GMU.liftIO $ do
            hPutStrLn stderr $ "TOKENS\n" ++ concatMap showToken toks
            writeBChan eventChannel $ E.Tokens toks

          PFailed srcspan msg -> do
            GMU.liftIO $ print $ show srcspan
            GMU.liftIO $
              do putStrLn "Lexer Error:"
                 print $ mkPlainErrMsg flags srcspan msg
  return ()

showToken :: GenLocated SrcSpan Token -> String
showToken t = "\nsrcLoc: " ++ srcloc ++ "\ntok: " ++ tok ++ "\n"
  where
    srcloc = show $ getLoc t
    tok = show $ unLoc t

showTokenWithSource :: (Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++
  tok ++ "\n" ++ "Source: " ++ src ++ "\n" ++ "Location: " ++ srcloc ++ "\n\n"
  where
    tok = show $ unLoc loctok
    srcloc = show $ getLoc loctok
