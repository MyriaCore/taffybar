-- | A simple module for declaratively putting elements
-- in a horizontal box. The polling box can be used when
-- the number of children in the box is not garunteed. 
module System.Taffybar.Widget.Generic.Box 
  ( boxNew
  , pollingBoxNew
  ) where

-- import GHC.Int (Int32(..))
import qualified Data.Text as T
import qualified Text.Regex as R
import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad ( forever )
import Control.Monad.IO.Class as M
import Control.Exception as E
import qualified GI.Gtk as G
import qualified GI.Gtk.Objects.Box as B
import GI.Gtk.Objects.Container ( containerGetChildren )
import System.Taffybar.Util

-- | Creates a horizontal box widget. 
-- %children% is a list of GTK widgets to add
-- as children to this box widget. 
boxNew :: (MonadIO m, G.IsWidget widget) => [ widget ] -> m G.Widget
boxNew children = putInBox children >>= G.toWidget

-- | Creates a polling horizontal box widget
-- by replacing the box's children with new ones
-- from the IO action. 
--
-- %children% is a list of GTK widgets to add
-- as children to this box widget. 
-- %interval% is the time between polls in seconds.
-- %cmd% is an IO action that should return a list
-- of /new/ children. This IO action will be run each
-- poll. 
pollingBoxNew
  :: (MonadIO m, G.IsWidget widget)
  => [ widget ]    -- ^ Initial children
  -> Double        -- ^ Update Interval (in seconds)
  -> IO [ widget ] -- ^ IO Action that returns new children
  -> m G.Widget
pollingBoxNew children interval cmd = liftIO $ do
  box <- putInBox children
  _ <- G.onWidgetRealize box $ do
    _ <- forkIO $ forever $ do
      let tryUpdate = do
            postGUIASync $ do
              box' <- cmd >>= replaceInBox box
              return ()
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (interval * 1000000)
    return ()
  G.toWidget box

-- | Puts the provided GTK widgets into a box together.
putInBox :: (MonadIO m, G.IsWidget child) => [ child ] -> m G.Box
putInBox widgets = liftIO $ do
  box <- G.boxNew G.OrientationHorizontal 0
  mapM_ (\c -> G.boxPackStart box c False False 0) widgets
  G.widgetShowAll box
  return box

-- | Replaces a GTK Box's children.
replaceInBox :: G.IsWidget child => G.Box -> [ child ] -> IO G.Box
replaceInBox box newChildren = do
  containerGetChildren box >>= mapM_ (\child -> G.containerRemove box child)
  mapM_ (\child -> G.containerAdd box child) newChildren
  G.widgetShowAll box
  return box

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()
