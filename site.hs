--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import System.FilePath (replaceExtension)

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

defaultDate = constField "date" "2019-01-01"

-- thanks to https://github.com/gibiansky/blog/blob/668a5bf7ae6815a20dd6d57c900318e34c959c13/Compilers.hs
compileWithFilter :: String -> [String] -> Item String -> Compiler (Item String)
compileWithFilter cmd args = withItemBody (unixFilter cmd args)

-- TODO hmm, that should probably be pre commit hook for content git repo?
-- wouldn't hurt to run again and check?
-- python3 ./ipynb_output_filter.py <lagr.ipynb >lagr2.ipynb 

ipynbFilterOutput :: Item String -> Compiler (Item String)
ipynbFilterOutput = compileWithFilter cmd args
  where cmd = "python3"
        args = [ "/L/Dropbox/repos/ipynb_output_filter/ipynb_output_filter.py" ]

ipynbRun :: Item String -> Compiler (Item String)
ipynbRun = compileWithFilter command arguments
  where command = "jupyter"
        arguments = ["nbconvert"
                    , "--execute"
                    , "--to", "markdown"
                    , "--stdin"
                    , "--stdout"
                    ]

-- (<&>)  = flip fmap
-- infixl 1 <&>


renameItem :: (String -> String) -> Item a -> Item a
renameItem f i =  i { itemIdentifier = new_id } where
  old_id = itemIdentifier i
  old_version = identifierVersion old_id
  new_id = setVersion old_version $ (fromFilePath $ f $ toFilePath old_id)

-- TODO ugh itemIdentifier is not exported???
-- renameItem f x = x { itemIdentifier = new_id } where
--   old_id = itemIdentifier x -- TODO do I need lens?..
--   old_path = identifierPath old_id
--   new_path = f old_path
--   new_id = old_id { identifierPath = new_path }

ipynbCompile i = do
  ipy <- ipynbRun i  -- x <&> (renameItem (\f -> replaceExtension f ".md")) -- ipynbFilterOutput >> ipynbRun
  let ipy_md = renameItem (\f -> replaceExtension f ".md") ipy -- change the extension to trick pandoc...
  pandoc <- readPandoc ipy_md
  let html = writePandoc pandoc
  let res = renameItem (\f -> replaceExtension f ".ipynb") html 
  return res

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler


    -- TODO shit this is problematic for all simple web servers, they think it's octet-stream :(
    let simpleRoute =
          gsubRoute "content/" (const "")
          `composeRoutes` setExtension "html" -- TODO fucking hell it's annoying. couldn't force github pages or preview server to support that
          -- `composeRoutes` setExtension "" -- TODO fucking hell it's annoying. couldn't force github pages or preview server to support that

    match (fromList ["meta/site.md", "meta/me.md"]) $ do
        route   $ gsubRoute "meta/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- TODO think how to infer date?
    match "content/**.md" $ do
        route   simpleRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

-- TODO in org mode files, date should be present
-- if it's not, complain, but the whole thing shouldn't fail!


    -- TODO not sure if I really want it to extract stuff automatically
    -- TODO how to refer to my stuff in other directories? Maybe some sort of private sync?
    -- TODO content should def. be a separate repository
    -- TODO make a script to check that links are reachable
    -- TODO disqus??
    -- TODO posts/etc is lame, use top level
    -- TODO rss
    -- TODO tags would be nice...
    match "content/**.ipynb" $ do
        route $ setExtension "html"
        compile $ getResourceString
              >>= ipynbCompile
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

          -- >>= readPandoc <&> writePandoc
          -- >>= writePandoc
          -- >>= loadAndApplyTemplate "templates/post.html"    postCtx
          -- TODO pandocCompiler
          --   >>= loadAndApplyTemplate "templates/post.html"    postCtx

    -- TODO appendIndex??https://github.com/aherrmann/jekyll_style_urls_with_hakyll_examples/blob/master/site.hs
  


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


    match "meta/index.html" $ do
        route   $ gsubRoute "meta/" (const "")
        compile $ do
            -- TODO extract in a variable or something
            posts <- loadAll ("content/**.md" .||. "content/**.ipynb") -- recentFirst =<< loadAll "posts/**.md" -- TODO recentFirst -- TODO actually, list all..
            let indexCtx =
                    listField "posts" postCtx (return posts)
                    <> constField "title" "Home"
                    <> defaultContext

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


-- https://github.com/karlicoss/karlicoss.github.io ???
