-- | This module provides a framework for creating GTK
-- widgets that have a mix of text labels and icons. 
--
-- The text provided to the functions in this module
-- will be parsed for a new <icon> markup tag. <icon>
-- tags must contain the name of a valid GTK icon. 
--
-- If found, these icon tags will be replaced with
-- their icon.
module System.Taffybar.Widget.Generic.IconLabel 
  ( iconLabelWidgetNew
  , pollingIconLabelWidgetNew 
  ) where

import qualified Data.Text as T
import qualified Text.Regex as R
import Control.Monad.IO.Class
import GI.Gtk hiding (Label, boxNew)
import System.Taffybar.Widget.Generic.Icon
import System.Taffybar.Widget.Generic.Box


-- | Creates a GTK widget with the specified Text.
--
-- If <icon> tags appear anywhere in the text, then
-- the GTK icon with the surrounding name will be
-- inserted inline alongside the label.
--
-- For example, the text "<icon>attention</icon> Warning!"
-- will produce a GTK widget with the attention icon on
-- the left, and a label with the text " Warning!" on the 
-- right.
iconLabelWidgetNew :: MonadIO m => T.Text -> m Widget
iconLabelWidgetNew text =
  let parsedText = map (fmap T.pack) $ parseIconEmbeddedText $ T.unpack text
  in sequence (map iconLabelToWidget parsedText) >>= boxNew

-- | Create a polling GTK Widget with the specified Text.
--
-- If %<icon>% tags appear anywhere in the text, then
-- the GTK icon with the surrounding name will be
-- inserted inline alongside the label.
--
-- For example, the text %<icon>attention</icon> Warning!%
-- will produce a GTK widget with the attention icon on
-- the left, and a label with the text " Warning!" on the 
-- right.
--
-- %text% is the initial text for the icon-label.
-- %interval% is the polling period in seconds.
-- %cmd% is an IO action that will be used to update
-- the icon-label. 
pollingIconLabelWidgetNew
  :: MonadIO m 
  => T.Text     -- ^ Initial text
  -> Double     -- ^ Update interval (in seconds)
  -> IO T.Text  -- ^ IO action that returns an updated label
  -> m Widget
pollingIconLabelWidgetNew text interval cmd =
  let iconLabels t = map (fmap T.pack) $ parseIconEmbeddedText $ T.unpack t
      widgets il = sequence (map iconLabelToWidget il)
      updateIconLabels = (iconLabels <$> cmd) >>= widgets
  in widgets (iconLabels text) >>= \ws -> pollingBoxNew ws interval updateIconLabels

-- | A datatype we'll use to parse icons out of the text. 
data IconLabel a = Icon a | Label a
  deriving (Read, Show, Eq, Ord)

instance Functor IconLabel where
  fmap f (Icon i) = Icon $ f i 
  fmap f (Label l) = Label $ f l

-- | Turns an individual IconLabel into a GTK widget.
-- Icons will be turned into GTK Image/Icon widgets,
-- and Labels will be turned into GTK Labels.
iconLabelToWidget :: (MonadIO io) => IconLabel T.Text -> io Widget
iconLabelToWidget (Icon i) = iconImageWidgetNewFromName i
iconLabelToWidget (Label l) = labelNew (Just l) >>= toWidget

-- | Takes a string and returns a list of icon names or labels.
--
-- >>> parseIconEmbeddedText "<icon>attention</icon> Warning!"
-- [Icon "attention", Label " Warning!"]
parseIconEmbeddedText :: String -> [IconLabel String]
parseIconEmbeddedText text = 
  case R.matchRegexAll (R.mkRegex "<icon>([^<>]+)</icon>") text of
    Nothing -> [Label text]
    Just (_, _, _, []) -> [Label text]
    Just ("", _, unparsed, icon:_) ->
      Icon icon : parseIconEmbeddedText unparsed
    Just (preceding, _, unparsed, icon:_) ->
      [ Label preceding, Icon icon ] ++ parseIconEmbeddedText unparsed
