import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Gdk.EventM
import qualified Graphics.UI.Gtk.Gdk.Events as E
import Control.Monad.Trans
import VKForm
import VKTemplate

main :: IO ()
main = do
  _ <- initGUI
  builder <- builderNew
  builderAddFromFile builder "MainWindow.ui"
  window <- builderGetObject builder castToWindow "mainWindow"
  viewport <- builderGetObject builder castToViewport "fieldsViewport"
  vbox <- vBoxNew True 10
  containerAdd viewport vbox
  createInputs $ toBox vbox
  on window deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  mainGUI


createInputs ::  Box -> IO ()
createInputs parent = do
  entries <- mapM _toInput $ getFields largerTemplate
  mapM_ (pack parent) $ entries

pack layout item = boxPackStart layout item PackNatural 0
