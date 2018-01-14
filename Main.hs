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

  -- load ui
  builderAddFromFile builder "MainWindow.ui"
  window <- builderGetObject builder castToWindow "mainWindow"
  viewport <- builderGetObject builder castToViewport "fieldsViewport"
  frame <- builderGetObject builder castToContainer "templateAlignment"
  fFilter <- builderGetObject builder castToFileFilter "templateFilter"
  generateButton <- builderGetObject builder castToButton "generateButton"

  -- "Load templates" button setup
  templateButton <- fileChooserButtonNew "Select template" FileChooserActionOpen
  fileChooserAddFilter (toFileChooser templateButton) fFilter
  containerAdd frame templateButton


  -- bind callbacks for static ui objects
  on window deleteEvent $ liftIO mainQuit >> return False

  let getTemplateFilePath = (getFieldData $ castToWidget templateButton)

  on templateButton fileChooserButtonFileSet
    $ liftIO $ onTemplateSelected viewport generateButton getTemplateFilePath
    >> widgetShowAll window

  widgetShowAll window
  mainGUI

-- callback function for loading a template file
onTemplateSelected container generateButton templateFile = do
  -- clear container
  containerForeach container $ containerRemove container
  -- create vbox for the dynamic fields
  vbox <- vBoxNew True 10
  containerAdd container vbox
  -- load template from file
  template <- templateFile >>= loadTemplateFromPath . (drop 7)
  -- generate fields
  fieldsInputs <- createInputs (toBox vbox) template
  -- rebind generateButton event
  on generateButton buttonReleaseEvent $ tryEvent $ liftIO $ generateHandler template fieldsInputs
  return ()


generateHandler layout fields = do
  e <- getFieldsData fields
  print $ fromFields layout e


loadTemplateFromPath path = do
  defaultTmp <- loadTemplate path
  return $ case (defaultTmp) of
    Right r -> r
    Left error -> MetaLabel {message = "Can't load file"}  -- display error


defaultTemplate = loadTemplateFromPath "template-guide.vkt"
