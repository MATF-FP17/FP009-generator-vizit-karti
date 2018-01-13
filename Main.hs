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
  frame <- builderGetObject builder castToContainer "templateAlignment"
  fFilter <- builderGetObject builder castToFileFilter "templateFilter"

  generateButton <- builderGetObject builder castToButton "generateButton"
  templateButton <- fileChooserButtonNew "Select template" FileChooserActionOpen

  fileChooserAddFilter (toFileChooser templateButton) fFilter

  containerAdd frame templateButton

  on window deleteEvent $ liftIO mainQuit >> return False
  on templateButton fileChooserButtonFileSet
    $ liftIO $ onTemplateSelected viewport generateButton (getFieldData $ castToWidget templateButton)
    >> widgetShowAll window

  widgetShowAll window
  mainGUI


onTemplateSelected container generateButton templateFile = do
  containerForeach container $ containerRemove container
  vbox <- vBoxNew True 10
  containerAdd container vbox
  template <- templateFile >>= templateLoader . (drop 7)
  fieldsInputs <- createInputs (toBox vbox) template
  on generateButton buttonReleaseEvent $ tryEvent $ liftIO $ getValuesHandler fieldsInputs
  return ()

getValuesHandler fields = do
  e <- getFieldsData fields
  print e


templateLoader path = do
  defaultTmp <- loadTemplate path
  return $ case (defaultTmp) of
    Right r -> r
    Left error -> MetaLabel {message = "Can't load file"}

defaultTemplate = templateLoader "template-guide.vkt"
