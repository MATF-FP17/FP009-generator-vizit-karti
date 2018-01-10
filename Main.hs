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
  generateButton <- builderGetObject builder castToButton "generateButton"
  vbox <- vBoxNew True 10
  containerAdd viewport vbox

  template <- defaultTemplate
  fieldsInputs <- createInputs (toBox vbox) template

  on window deleteEvent $ liftIO mainQuit >> return False
  on generateButton buttonReleaseEvent $ tryEvent $ liftIO $ getValuesHandler fieldsInputs 

  widgetShowAll window
  mainGUI


getValuesHandler fields = do
  e <- getFieldsData fields
  print e

defaultTemplate = do
  defaultTmp <- loadTemplate "template-guide.vkt"
  return $ case (defaultTmp) of Right r -> r
