import Graphics.UI.Gtk
import VKForm
import VKTemplate

main :: IO ()
main = do
  _ <- initGUI
  window <- windowNew
  vbox <- vBoxNew True 10
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerBorderWidth := 10, containerChild := vbox]
  createInputs $ toBox vbox
  _ <- onDestroy window mainQuit
  widgetShowAll window
  mainGUI


createInputs ::  Box -> IO ()
createInputs parent = do
  entries <- mapM _toInput $ getFields largerTemplate
  mapM_ (pack parent) $ entries

pack layout item = boxPackStart layout item PackNatural 0
