module Main (main) where

import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (
  MonadIO (liftIO),
  managed,
  managed_,
  runManaged,
 )
import DearImGui (
  button,
  createContext,
  destroyContext,
  getDrawData,
  newFrame,
  render,
  showDemoWindow,
  text,
  withWindowOpen,
 )
import DearImGui.OpenGL3 (
  openGL3Init,
  openGL3NewFrame,
  openGL3RenderDrawData,
  openGL3Shutdown,
 )
import DearImGui.SDL (
  pollEventWithImGui,
  sdl2NewFrame,
  sdl2Shutdown,
 )
import DearImGui.SDL.OpenGL (sdl2InitForOpenGL)
import Graphics.GL (glClear, pattern GL_COLOR_BUFFER_BIT)
import SDL (
  Event (eventPayload),
  EventPayload (QuitEvent),
  Window,
  WindowConfig (windowGraphicsContext),
  WindowGraphicsContext (OpenGLContext),
  createWindow,
  defaultOpenGL,
  defaultWindow,
  destroyWindow,
  glCreateContext,
  glDeleteContext,
  glSwapWindow,
  initializeAll,
 )

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged do
    -- Create a window using SDL.
    -- As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let
        title = "Hello, Dear ImGui!"
        config =
          defaultWindow
            { windowGraphicsContext =
                OpenGLContext defaultOpenGL
            }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    liftIO $ mainLoop window

mainLoop :: Window -> IO ()
mainLoop window =
  let
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent
   in
    unlessQuit do
      -- Tell ImGui we're starting a new frame
      openGL3NewFrame
      sdl2NewFrame
      newFrame

      -- Build the GUI
      withWindowOpen "Hello, ImGui!" do
        -- Add a text widget
        text "Hello, ImGui!"

        -- Add a button widget, and call 'putStrLn' when it's clicked
        button "Clickety Click" >>= \case
          False -> return ()
          True -> putStrLn "Ow!"

      -- Show the ImGui demo window
      showDemoWindow

      -- Render
      glClear GL_COLOR_BUFFER_BIT

      render
      openGL3RenderDrawData =<< getDrawData

      glSwapWindow window

      mainLoop window
