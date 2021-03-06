{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE BangPatterns          #-}

-- The editor's 'HandleEvent' instance handles a set of basic input
-- events that should suffice for most purposes; see the source for a
-- complete list.
module EditRope
  ( Editor(editContents, editorName)
  -- * Constructing an editor
  , editor
  -- * Reading editor contents
  , getEditContents
  -- * Handling events
  , handleEditorEvent
  -- * Editing text
  -- , applyEdit
  , applyComposed
  -- * Lenses for working with editors
  , editContentsL
  -- * Rendering editors
  , renderEditor
  -- * Attributes
  , editAttr
  , editFocusedAttr
  , TokenizedEvent(..)
  -- for Spec.hs
  , renderTokensForLine
  , renderTokensForLineReverse
  , getAttr
  , fixEmptyLines
  , showToken
  )
where

import           Data.Monoid
import           Graphics.Vty           (Event (..), Key (..), Modifier (..))
import           Lens.Micro

import           Data.List
import           Data.Maybe
import qualified Graphics.Vty           as V
import qualified Yi.Rope                as Y

import           Brick.AttrMap
import           Brick.Types
import           Brick.Widgets.Core

import           Control.Monad.IO.Class
import           Data.Data
import qualified GHC
import qualified Lexer                  as GHC
import qualified FastString             as GHC
import qualified SrcLoc                 as GHC

import           Control.DeepSeq
import qualified Data.Text              as T
import           Debug.Trace

import Types


newtype TokenizedEvent a = Tokens a
  deriving Show

type Loc = (Int, Int)

data Operation =
  InsertChar Char Loc
  | DeleteChar Loc
  | MoveCursor Loc
  | Undo
  | HandleTokens [GHC.Located GHC.Token]

instance Show Operation where
    show op = case op of
      InsertChar _ position -> "InsertChar " ++ show position
      DeleteChar position   -> "DeleteChar " ++ show position
      MoveCursor d          -> "MoveCursor " ++ show d
      Undo                  -> "Undo"
      HandleTokens ts       -> "HandleTokens " ++ concatMap showToken ts

showToken :: GHC.GenLocated GHC.SrcSpan GHC.Token -> String
showToken t = "\nsrcLoc: " ++ srcloc ++ "\ntok: " ++ tok ++ "\n"
  where
    srcloc = show $ GHC.getLoc t
    tok = show $ GHC.unLoc t

-- Data, Typeable for GHC.Token
deriving instance Data GHC.Token
deriving instance Typeable GHC.Token

tokenAsString :: GHC.Token -> String
tokenAsString = show . toConstr

-- | Editor state.  Editors support the following events by default:
--
-- * Ctrl-a: go to beginning of line
-- * Ctrl-e: go to end of line
-- * Ctrl-d, Del: delete character at cursor position
-- * Backspace: delete character prior to cursor position
-- * Ctrl-k: delete all from cursor to end of line
-- * Ctrl-u: delete all from cursor to beginning of line
-- * Arrow keys: move cursor
-- * Enter: break the current line at the cursor position
data Editor n =
    Editor { editContents   :: Y.YiString
           -- ^ The contents of the editor
           , editorName     :: n
           -- ^ The name of the editor
           , editCursor     :: Loc
           -- TODO: undo will be inverse of operation, depending on increment/decrement of index into this list while un- or re-doing
           , editOperations :: [Operation]
           -- TODO: render tokens
           , editTokens     :: [GHC.Located GHC.Token]
           , editSendSource :: String -> IO ()
           }
suffixLenses ''Editor

-- | Construct an editor over 'String' values
editor ::
       n
       -- ^ The editor's name (must be unique)
       -> Y.YiString
       -- ^ The initial content
       -> (String -> IO ())
       -> Editor n
editor name s = Editor s name (0, 0) [] []

-- TODO orphane instance !
instance TextWidth Y.YiString where
  textWidth = V.wcswidth . Y.toString

instance (Show n) => Show (Editor n) where
    show e =
        concat [ "Editor { "
               , "editContents = " <> show ((Y.toString.editContents) e)
               , ", editorName = " <> show (editorName e)
               , ", cursorPos = " <> show (editCursor e)
               , ", operations = " <> show (editOperations e)
              -- TODO , ", tokens = " <> show (editTokens e)
               , "}"
               ]

instance Named (Editor n) n where
    getName = editorName


-- TODO
handleTokens :: [GHC.Located GHC.Token] -> (Editor n -> Editor n)
handleTokens ts = editTokensL .~ ts

moveCursor :: Loc -> (Editor n -> Editor n)
moveCursor (column, line) =
  (editCursorL %~ moveColumn column) . (editCursorL %~ moveLine line)
  where
    moveColumn newColumn (_, l) = (newColumn, l)
    moveLine newLine (c, _) = (c, newLine)

-- TODO refactor insertCh + deleteCh
insertCh :: Char -> Loc -> (Editor n -> Editor n)
insertCh ch position = editContentsL %~ insertChar ch position
  where
    insertChar :: Char -> Loc -> (Y.YiString -> Y.YiString)
    insertChar char (c, l) s =
      let (lBefore, lAfter) = Y.splitAtLine l s
          (cBefore, cAfter) = Y.splitAt c lAfter
      in lBefore <> (cBefore <> Y.cons char cAfter)

deleteCh :: Loc -> (Editor n -> Editor n)
deleteCh position = editContentsL %~ deleteChar position
  where
    deleteChar :: Loc -> (Y.YiString -> Y.YiString)
    deleteChar (c, l) s =
      let (lBefore, lAfter) = Y.splitAtLine l s
          (cBefore, cAfter) = Y.splitAt c lAfter
          cTail = fromMaybe Y.empty $ Y.tail cAfter
      in lBefore <> (cBefore <> cTail)

getLineLength :: Int -> Y.YiString -> Int
getLineLength l = Y.length . Y.takeWhile (/= '\n') . snd . Y.splitAtLine l

defaultDisplayRegion :: V.DisplayRegion
defaultDisplayRegion = (0, 0)

handleEditorEvent :: Ord n => BrickEvent n (TokenizedEvent [GHC.Located GHC.Token]) -> Editor n -> EventM n (Editor n)
handleEditorEvent e ed = do

        let cp@(column, line) = ed ^. editCursorL
            contents = editContents ed

            -- TODO refactor NoOp to Maybe Op
            (contentOp, cursorOp, metaOp) = case e of
                  -- EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
                  -- EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
                  -- EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
                  -- EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
                  -- EvKey (KChar 'u') [MCtrl] -> Z.killToBOL

                  VtyEvent (EvKey (KChar 'z') [MCtrl])                              -> (Just Undo, Nothing, Nothing)

                  VtyEvent (EvKey KDel [])      | Y.length contents > 0             -> (Just $ DeleteChar cp, Nothing, Nothing)

                  -- TODO enable move to next line
                  VtyEvent (EvKey KBS [])       | cp /= (0, 0)                      -> (Just $ DeleteChar (max 0 (column - 1), line), Just $ MoveCursor (max 0 (column - 1), line), Nothing)

                  VtyEvent (EvKey KEnter [])                                        -> (Just $ InsertChar '\n' cp, Just $ MoveCursor (0, line + 1), Nothing)

                  VtyEvent (EvKey (KChar c) []) | c /= '\t'                         -> (Just $ InsertChar c cp, Just $ MoveCursor (column + 1, line), Nothing)

                  VtyEvent (EvKey KUp [])       | line > 0                          -> (Nothing, Just $ MoveCursor (min (getLineLength (line - 1) contents) column, line - 1), Nothing)
                  VtyEvent (EvKey KDown [])     | line < Y.countNewLines contents   -> (Nothing, Just $ MoveCursor (min (getLineLength (line + 1) contents) column, line + 1), Nothing)

                  -- TODO enable move to next line
                  VtyEvent (EvKey KLeft [])                                         -> (Nothing, Just $ MoveCursor (max 0 (column - 1), line), Nothing)
                  -- TODO enable move to next line
                  VtyEvent (EvKey KRight [])                                        -> (Nothing, Just $ MoveCursor (min (getLineLength line contents) (column + 1), line), Nothing)

                  AppEvent (Tokens ts) -> (Nothing, Nothing, Just $ HandleTokens ts)

                  _ -> (Nothing, Nothing, Nothing)

            ed' = applyComposed [contentOp, cursorOp, metaOp] ed

        -- liftIO $ hPutStrLn stderr $ "operations: " ++ show (ed' ^. editOperationsL)

        case contentOp of
          Nothing -> return ed'
          -- TODO: call lexer only for actual changes to contents, not just change-operations
          Just op -> sendToLexer $ consOp ed' op

sendToLexer :: Editor n -> EventM n (Editor n)
sendToLexer ed = do
  liftIO $ editSendSource ed $ (Y.toString . editContents) ed
  return ed

consOp :: Editor n -> Operation -> Editor n
consOp e op = e & editOperationsL %~ (\l -> op : l)

applyComposed :: [Maybe Operation] -> Editor n -> Editor n
applyComposed fs ed = foldl' foldOperation ed fs

foldOperation :: Editor n -> Maybe Operation -> Editor n
foldOperation e op =
  case op of
    -- changing contents
    Just (InsertChar ch position) -> e & insertCh ch position
    Just (DeleteChar position)    -> e & deleteCh position
    Just Undo                     -> e & id -- TODO
    -- changing cursor
    Just (MoveCursor d)           -> e & moveCursor d
    -- changing meta
    Just (HandleTokens ts)        -> e & handleTokens ts
    _                             -> e


-- | The attribute assigned to the editor when it does not have focus.
editAttr :: AttrName
editAttr = "edit"

-- | The attribute assigned to the editor when it has focus. Extends
-- 'editAttr'.
editFocusedAttr :: AttrName
editFocusedAttr = editAttr <> "focused"

-- | Get the contents of the editor.
getEditContents :: Editor n -> Y.YiString
getEditContents = editContents

-- | Turn an editor state value into a widget. This uses the editor's
-- name for its scrollable viewport handle and the name is also used to
-- report mouse events.
renderEditor :: (Ord n, Show n)
             => Bool
             -- ^ Whether the editor has focus. It will report a cursor
             -- position if and only if it has focus.
             -> Editor n
             -- ^ The editor.
             -> Widget n
renderEditor focus e =
    let cp@(column, line) = e ^. editCursorL
        contents = e ^. editContentsL
        toLeft = Y.take column $ snd $ Y.splitAtLine line contents
        cursorLoc = Location (Y.length toLeft, line)
        -- cursorLoc = Location (textWidth $ Y.toString toLeft, line)
        -- atChar = charAtCursor cp $ e ^. editContentsL
        -- atCharWidth = maybe 1 textWidth atChar

    in withAttr editFocusedAttr $
       viewport (e^.editorNameL) Both $
    --    clickable (e^.editorNameL) $
       (if focus then showCursor (e^.editorNameL) cursorLoc else id) $
       visibleRegion cursorLoc (1, 1) $
    --    visibleRegion cursorLoc (atCharWidth, 1) $
       renderTokens (editTokens e) (getEditContents e)

-- charAtCursor :: (Int, Int) -> Y.YiString -> Maybe String
-- charAtCursor (column, line) s =
--   let toRight = snd $ Y.splitAt column (snd $ Y.splitAtLine line s)
--   in fmap (replicate 1) (Y.head toRight)

mkLocatedToken :: Int -> Int -> Int -> Int -> GHC.Token -> GHC.Located GHC.Token
mkLocatedToken lin1 col1 lin2 col2 = 
  GHC.L (GHC.mkSrcSpan 
    (GHC.mkSrcLoc (GHC.fsLit "") lin1 col1)
    (GHC.mkSrcLoc (GHC.fsLit "") lin2 col2))

splitMultilineTokens :: [Token] -> [Token]
splitMultilineTokens =
  concatMap splitToken 
  where
    splitToken :: Token -> [Token]
    splitToken token = 
      let (GHC.RealSrcSpan location) = GHC.getLoc token
          [l1, c1, l2, c2] = sub1 <$> ([GHC.srcSpanStartLine, GHC.srcSpanStartCol, GHC.srcSpanEndLine, GHC.srcSpanEndCol] <*> [location])
          theToken = GHC.unLoc token
      in split l1 c1 l2 c2 theToken
        where
          split l1 c1 l2 c2 theToken
            | l2 - l1 > 1 = [firstToken] ++ [mkLocatedToken l 1 0 0 theToken | l <- [l1+1..l2-1]] ++ [lastToken]
            | l2 - l1 > 0 = [firstToken, lastToken]
            | otherwise = [token]
            where
              firstToken = mkLocatedToken l1 c1 0 0 theToken
              lastToken  = mkLocatedToken l2 1 l2 c2 theToken

-- splitLineTokens :: Int -> [Token] -> ([Token], [Token])
-- splitLineTokens l = span (compareLine (l >=))
-- 
-- compareLine :: (Int -> Bool) -> Token -> Bool
-- compareLine f token =
--   let (GHC.RealSrcSpan location) = GHC.getLoc token
--       l1 = GHC.srcSpanStartLine location
--   in f l1

getLineStart :: Token -> Int
getLineStart token =
  let (GHC.RealSrcSpan location) = GHC.getLoc token
  in GHC.srcSpanStartLine location

fixEmptyLines :: [T.Text] -> [[Token]] -> [(T.Text, [Token])]
fixEmptyLines ls tss =
  -- TODO: fix it
  zip ls tss
  
tSpace :: T.Text
tSpace = " "

-- copied from Brick.Widget.Core str
renderTokens :: [GHC.Located GHC.Token] -> Y.YiString -> Widget n
renderTokens ts str =
    Widget Fixed Fixed $ do
      c <- getContext
      let attrMap = c ^. ctxAttrMapL
          theLines = fixEmpty . Y.toText <$> Y.lines str
          fixEmpty l = if l == T.empty
                        then tSpace
                        else l

          splitTokens = splitMultilineTokens ts
          
          -- TODO 2: optimization ? ...let lists of tokens be reverted; groupBy is NOT reverted !
          lineTokens = groupBy (\t1 t2 -> getLineStart t1 == getLineStart t2) splitTokens
          -- !tmp = trace ("DEBUG " ++ concat (showToken <$> concat lineTokens)) lineTokens
          
          -- TODO: fix tokens for empty lines: fixEmptyLines
          linesWithTokens = fixEmptyLines theLines lineTokens
          
      case force theLines of
          [] -> return emptyResult

          [oneLine] -> return $ emptyResult & imageL .~ renderTokensForLine attrMap ts oneLine

          -- TODO: render each token with it's correct attributes
          multipleLines ->
              let maxLength = maximum $ T.length <$> multipleLines
                  lineImgs = lineImg <$> linesWithTokens
                  lineImg (lStr, ts) = renderTokensForLine attrMap ts $ lStr <> T.replicate (maxLength - T.length lStr) tSpace
              in return $ emptyResult & imageL .~ V.vertCat lineImgs


getAttr :: AttrMap -> String -> V.Attr
getAttr m name = attrMapLookup (attrName name) m

sub1 :: Int -> Int
sub1 x = x - 1

subText :: Int -> Int -> T.Text -> T.Text
subText pos len text = (T.take len . T.drop pos) text

renderTokensForLine :: AttrMap -> [GHC.Located GHC.Token] -> T.Text -> V.Image
renderTokensForLine attrMap ts text =
  
  let (pos, imgs) = foldl' foldIt (0, []) ts
      textEnd = T.drop pos text
  in V.horizCat . reverse $ V.text' (getAttr attrMap "WS") textEnd : imgs
  
  where 
    foldIt :: (Int, [V.Image]) -> GHC.Located GHC.Token -> (Int, [V.Image])
    foldIt (pos, theImages) theToken = 
      let (GHC.RealSrcSpan location) = GHC.getLoc theToken
          [c1, c2] = [sub1 . GHC.srcSpanStartCol, sub1 . GHC.srcSpanEndCol] <*> [location]
          tokenStr = tokenAsString (GHC.unLoc theToken)
          tokenLen = c2 - c1
          tokenImage = V.text' (getAttr attrMap tokenStr) (subText c1 tokenLen text)
          diff = c1 - pos
          newImages =
            if diff > 0
              then [tokenImage, V.text' (getAttr attrMap "WS") (subText pos diff text)]
              else [tokenImage]
      in (c2, newImages <> theImages)

renderTokensForLineReverse :: AttrMap -> [GHC.Located GHC.Token] -> T.Text -> V.Image
renderTokensForLineReverse attrMap ts text =
  
  let (pos, imgs) = foldl' foldIt (T.length text, []) ts
      textStart = T.take pos text
  in V.horizCat $ V.text' (getAttr attrMap "WS") textStart : imgs
  
  where 
    foldIt :: (Int, [V.Image]) -> GHC.Located GHC.Token -> (Int, [V.Image])
    foldIt (pos, theImages) theToken = 
      let (GHC.RealSrcSpan location) = GHC.getLoc theToken
          [c1, c2] = [sub1 . GHC.srcSpanStartCol, sub1 . GHC.srcSpanEndCol] <*> [location]
          tokenStr = tokenAsString (GHC.unLoc theToken)
          tokenLen = c2 - c1
          tokenImage = V.text' (getAttr attrMap tokenStr) (subText c1 tokenLen text)
          diff = pos - c2
          newImages =
            if diff > 0
              then [tokenImage, V.text' (getAttr attrMap "WS") (subText c2 diff text)]
              else [tokenImage]
      in (c1, newImages <> theImages)

