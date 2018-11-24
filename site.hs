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
  extractDate Item {itemBody=Pandoc meta _} = return $ stringify $ docTitle meta -- TODO render to html properly?

pandocContext :: Context Pandoc
pandocContext = mempty <> orgFileTags <> orgAuthor <> orgTitle <> orgDate


-- TODO ugh. surely it can't be that ugly right?
data PandocX = PandocX Pandoc String

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

-- TODO in org mode files, date should be present
-- if it's not, complain, but the whole thing shouldn't fail!

-- https://github.com/turboMaCk/turboMaCk.github.io/blob/develop/site.hs#L61 ??
    match "posts/**.org" $ do
        route $ setExtension "html"
        compile $ do
          pandoc <- getResourceBody >>= readPandoc
          let rendered = writePandoc pandoc
          loadAndApplyTemplate "templates/post.html" myContext $ combineItems PandocX pandoc rendered

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
