--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Text.Pandoc (readOrg, Pandoc(..))
import Text.Pandoc.Options (def, writerVariables, writerTableOfContents)

import Hakyll.Web.Pandoc
import Debug.Trace

myContext = defaultContext <> field "ftags" (\p -> return "TODO FILETAGS") <> constField "author" "DDD"

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

-- TODO pandoc lisbon.org -s -o lisbon.html ok that works; also use pandoc -D html for help..
-- pandoc lisbon.org -s -o lisbon.html --metadata author='Dima'
  
-- TODO in org mode files, date should be present
-- if it's not, complain, but the whole thing shouldn't fail!

-- https://github.com/turboMaCk/turboMaCk.github.io/blob/develop/site.hs#L61 ??
    match "posts/**.org" $ do
        route $ setExtension "html"
        -- TODO yay!
        -- TODO right.. what about title and date???
        -- compile $ pandocCompiler -- WithTransform def (def {
                                 --            writerVariables = [("author", "Dima")],
                                 --            writerTableOfContents = True
             -- >>= loadAndApplyTemplate "templates/post.html"    myContext
        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html" myContext
        -- compile $ do
        --   body <- getResourceBody
        --   -- traceShowM body
        --   pd <- readPandoc body
        --   traceShowM pd
        --   let wp = writePandoc pd 
        --   -- right ok, so it's got meta in Pandoc thing.
        --   -- how come it's lost after writePandoc??
        --   traceShowM wp
          -- return wp
          -- res <- pandocCompiler
          -- traceShow res $ return res
  -- TODO ok, I suppose we need to get Compiler Pandoc first, extract meta from it; and only after that we get 
  -- TODO how to add date??
  -- is it parsing html to extact author??
            -- >>= loadAndApplyTemplate "templates/default.html" noteCtx
            -- >>= relativizeUrls

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Archives"            `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls


    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/**.org" -- TODO
    --         let indexCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Home"                `mappend`
    --                 defaultContext

    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
