{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- The editor's 'HandleEvent' instance handles a set of basic input
-- events that should suffice for most purposes; see the source for a
-- complete list.
module EditRope
  ( Editor(editContents, editorName, editDrawContents)
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
  , editDrawContentsL
  -- * Rendering editors
  , renderEditor
  -- * Attributes
  , editAttr
  , editFocusedAttr
  , TokenizedEvent(..)
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

import           GHC

import           Control.Monad.IO.Class



newtype TokenizedEvent a = Tokens a
  deriving Show

type Loc = (Int, Int)

data Operation =
  InsertChar Char Loc
  | DeleteChar Loc
  | MoveCursor Loc
  | Undo
  | HandleTokens [Located Token]

instance Show Operation where
    show op = case op of
      InsertChar _ position -> "InsertChar " ++ show position
      DeleteChar position   -> "DeleteChar " ++ show position
      MoveCursor d          -> "MoveCursor " ++ show d
      Undo                  -> "Undo"
      HandleTokens tokens   -> "HandleTokens " ++ concatMap showToken tokens

showToken :: GenLocated SrcSpan Token -> String
showToken t = "\nsrcLoc: " ++ srcloc ++ "\ntok: " ++ tok ++ "\n"
  where
    srcloc = show $ getLoc t
    tok = show $ unLoc t

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
    Editor { editContents     :: Y.YiString
           -- ^ The contents of the editor
           , editDrawContents :: Y.YiString -> Widget n
           -- ^ The function the editor uses to draw its contents
           , editorName       :: n
           -- ^ The name of the editor
           , editCursor       :: Loc
           -- TODO undo will be inverse of operation, depending on increment/decrement of index into this list while un- or re-doing
           , editOperations   :: [Operation]
           -- TODO render tokens
           , editTokens       :: [Located Token]
           , editSendSource   :: String -> IO ()
           }
suffixLenses ''Editor

-- | Construct an editor over 'String' values
editor ::
       n
       -- ^ The editor's name (must be unique)
       -> (Y.YiString -> Widget n)
       -- ^ The content rendering function
       -> Y.YiString
       -- ^ The initial content
       -> (String -> IO ())
       -> Editor n
editor name draw s = Editor s draw name (0, 0) [] []

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
handleTokens :: [Located Token] -> (Editor n -> Editor n)
handleTokens tokens = editTokensL .~ tokens

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

handleEditorEvent :: BrickEvent n (TokenizedEvent [Located Token]) -> Editor n -> EventM n (Editor n)
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

                  AppEvent (Tokens tokens) -> (Nothing, Nothing, Just $ HandleTokens tokens)

                  _ -> (Nothing, Nothing, Nothing)

            ed' = applyComposed [contentOp, cursorOp, metaOp] ed

        -- liftIO $ hPutStrLn stderr $ "operations: " ++ show (ed' ^. editOperationsL)
        case contentOp of
          Nothing -> return ed'
          -- TODO call lexer only for actual changes to contents, not just change-operations
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
    Just (HandleTokens tokens)    -> e & handleTokens tokens
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
getEditContents e = e^.editContentsL

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
        cursorLoc = Location (textWidth $ Y.toString toLeft, line)
        -- atChar = charAtCursor cp $ e ^. editContentsL
        atCharWidth = 1 -- maybe 1 textWidth atChar
    in withAttr (if focus then editFocusedAttr else editAttr) $
       viewport (e^.editorNameL) Both $
       clickable (e^.editorNameL) $
       (if focus then showCursor (e^.editorNameL) cursorLoc else id) $
       visibleRegion cursorLoc (atCharWidth, 1) $
       e^.editDrawContentsL $
       getEditContents e

-- charAtCursor :: (Int, Int) -> Y.YiString -> Maybe String
-- charAtCursor (column, line) s =
--   let toRight = snd $ Y.splitAt column (snd $ Y.splitAtLine line s)
--   in fmap (replicate 1) (Y.head toRight)

