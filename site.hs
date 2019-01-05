--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Text.Pandoc (readOrg, Pandoc(..), docTitle, docDate, Meta, Inline)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Options (def, writerVariables, writerTableOfContents)
import           Control.Applicative ((<|>))

import Hakyll.Web.Pandoc
import Debug.Trace

pandocMeta :: (Meta -> [Inline]) -> (Item Pandoc -> Compiler String)
pandocMeta extractor Item {itemBody=Pandoc meta _} = return $ stringify $ extractor meta -- TODO proper html??

-- TODO extract that stuff somewhere and share??
orgFileTags = field "filetags" (\p -> return "TODO FILETAGS")
orgAuthor = constField "author" "Dima" -- TODO docAuthors??
orgTitle = field "title" $ pandocMeta docTitle
orgDate = field "date" $ pandocMeta docDate

pandocContext :: Context Pandoc
pandocContext = orgFileTags <> orgAuthor <> orgTitle <> orgDate

-- TODO ugh. surely it can't be that ugly right?
data PandocX = PandocX Pandoc String

combineItems :: (a -> b -> c) -> Item a -> Item b -> Item c
combineItems f Item{itemBody=ba, itemIdentifier=ii} Item{itemBody=bb} = Item {itemBody=f ba bb, itemIdentifier=ii}

combineContexts :: Context Pandoc -> Context String -> Context PandocX
combineContexts (Context f) (Context g) = Context $ \k a Item{itemBody=PandocX pdoc rendered} -> f k a Item {itemBody=pdoc, itemIdentifier=""} <|> g k a Item {itemBody=rendered, itemIdentifier=""} -- TODO break down item ;

myContext :: Context PandocX
myContext = combineContexts pandocContext defaultContext

defaultDate = constField "date" "2019-09-01"

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- match (fromList ["about.rst", "contact.markdown"]) $ do
    --     route   $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

-- TODO in org mode files, date should be present
-- if it's not, complain, but the whole thing shouldn't fail!


    match "posts/**.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

-- https://github.com/turboMaCk/turboMaCk.github.io/blob/develop/site.hs#L61 ??
-- TODO reference to how to read my posts?? e.g. what todo states mean etc
    -- match "posts/**.org" $ do
    --     route $ setExtension "html"
    --     compile $ (do
    --       pandoc <- getResourceBody >>= readPandoc
    --       let rendered = writePandoc pandoc
    --       loadAndApplyTemplate "templates/post.html" myContext $ combineItems PandocX pandoc rendered
    --       ) >>= relativizeUrls

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


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/**.md" -- recentFirst =<< loadAll "posts/**.md"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    defaultDate <> -- TODO will it override metadata???
    defaultContext


-- TODO ipynb conversion -- markdown was a bit meh.. html kinda ok
