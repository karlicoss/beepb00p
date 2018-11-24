--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Text.Pandoc (readOrg, Pandoc(..), docTitle)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Options (def, writerVariables, writerTableOfContents)
import           Control.Applicative ((<|>))

import Hakyll.Web.Pandoc
import Debug.Trace

-- orgFileTags :: Context Pandoc
orgFileTags = field "filetags" (\p -> return "TODO FILETAGS")

orgAuthor = constField "author" "Dima"
orgTitle = field "title" (\p -> return "TITLE")
orgDate = field "date" extractDate where
  extractDate :: Item Pandoc -> Compiler String
  extractDate Item {itemBody=Pandoc meta _} = return $ stringify $ docTitle meta -- TODO render to html properly?? not sure 



pandocContext :: Context Pandoc
pandocContext = mempty <> orgFileTags <> orgAuthor <> orgTitle <> orgDate

data PandocX = PandocX Pandoc String


-- TODO readPandoc goes in first; after that writePandoc in second

combineItems :: (a -> b -> c) -> Item a -> Item b -> Item c
combineItems f Item{itemBody=ba, itemIdentifier=ii} Item{itemBody=bb} = Item {itemBody=f ba bb, itemIdentifier=ii}

combineContexts :: Context Pandoc -> Context String -> Context PandocX
combineContexts (Context f) (Context g) = Context $ \k a Item{itemBody=PandocX pdoc rendered} -> f k a Item {itemBody=pdoc, itemIdentifier=""} <|> g k a Item {itemBody=rendered, itemIdentifier=""} -- TODO break down item ;

myContext :: Context PandocX
myContext = combineContexts pandocContext defaultContext

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
        compile $ do
          pandoc <- getResourceBody >>= readPandoc -- Item Pandoc
            -- >>= loadAndApplyTemplate "templates/post.html" pandocContext
  -- TODO ok, so if we apply myContext agains pandoc, we'll get 
          let rendered = writePandoc pandoc -- Item String -- TODO ugh. I guess we wanna keep track of original Pandoc then??
          loadAndApplyTemplate "templates/post.html" myContext $ combineItems PandocX pandoc rendered
          -- return res
          -- return $ writePandoc pandoc
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
