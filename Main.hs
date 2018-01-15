import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Gdk.EventM
import qualified Graphics.UI.Gtk.Gdk.Events as E
import Control.Monad.Trans
import VKForm
import VKTemplate
import Diagram

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
  generateButtonFrame <- builderGetObject builder castToContainer "generateButtonFrame"

  -- "Load templates" button setup
  templateButton <- fileChooserButtonNew "Select template" FileChooserActionOpen
  fileChooserAddFilter (toFileChooser templateButton) fFilter
  containerAdd frame templateButton
  -- bind callbacks for static ui objects
  on window deleteEvent $ liftIO mainQuit >> return False

  let getTemplateFilePath = (getFieldData $ castToWidget templateButton)

  on templateButton fileChooserButtonFileSet
    $ liftIO $ onTemplateSelected viewport generateButtonFrame getTemplateFilePath
    >> widgetShowAll window

  widgetShowAll window
  mainGUI

-- callback function for loading a template file
onTemplateSelected container generateButtonFrame templateFile = do
  -- clear container and generateButton
  containerForeach container $ containerRemove container
  containerForeach generateButtonFrame $ containerRemove generateButtonFrame
  -- create vbox for the dynamic fields
  vbox <- vBoxNew True 10
  containerAdd container vbox
  -- recreate generate button
  gbutton <- buttonNewWithLabel "Generate"
  containerAdd generateButtonFrame gbutton
  -- load template from file
  template <- templateFile >>= loadTemplateFromPath . (drop 7)
  -- generate fields
  fieldsInputs <- createInputs (toBox vbox) template
  -- bind generation callback
  on gbutton buttonReleaseEvent $ liftIO $ generateHandler template fieldsInputs
    >> return False


generateHandler layout fields = getFieldsData fields >>= mainDiagram . (fromFields layout)

loadTemplateFromPath path = do
  defaultTmp <- loadTemplate path
  return $ case (defaultTmp) of
    Right r -> r
    Left error -> MetaLabel {message = "Can't load file"}  -- display error


defaultTemplate = loadTemplateFromPath "template-guide.vkt"
