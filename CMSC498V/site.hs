--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "lectures/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" defaultContext
            >>= relativizeUrls

    match (fromList 
              [ "index.markdown"
              , "schedule.markdown"
              , "schedule-private.markdown"
              , "resources.markdown"
              , "homeworks.markdown"
              , "project.markdown"
              ]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


config :: Configuration
config = defaultConfiguration {
          destinationDirectory = "../docs"
       }

