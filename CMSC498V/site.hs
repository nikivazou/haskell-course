--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "lectures/*.lhs" $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "homeworks/*.lhs" $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


myPandocCompiler ::  Compiler (Item String)
myPandocCompiler 
  = pandocCompiler {- With 
       defaultHakyllReaderOptions 
       (defaultHakyllWriterOptions ) -- {writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js"})
-} 

config :: Configuration
config = defaultConfiguration {
          destinationDirectory = "../docs"
       }

