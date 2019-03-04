--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Control.Applicative (empty)
import Control.Monad ((>=>))
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import System.FilePath (replaceExtension)

import Text.Pandoc (readOrg, Pandoc(..), docTitle, docDate, Meta, Inline)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Options (def, writerVariables, writerTableOfContents)
import           Control.Applicative ((<|>))

import Hakyll.Web.Pandoc
import Debug.Trace

baseUrl = "https://beepb00p.xyz"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Mildly entertaining"
    , feedAuthorName  = "karlicoss"
    , feedAuthorEmail = "karlicoss@gmail.com"
    , feedDescription = "feed" -- TODO ?
    , feedRoot        = baseUrl -- TODO how to test feed?
    }

  
-- TODO think about naming?
name2disqusid = [ ("me.md", "blog-me")
                ] :: [(String, String)]

data Overrides = Overrides { upid :: Maybe String, date :: Maybe String, title :: Maybe String, summary :: Maybe String }


defaultOverrides = Overrides { upid = Nothing, date = Nothing, title = Nothing, summary = Nothing }


-- TODO move these to external file?
overrides = [ ("meta/me.md"               , dovr { upid    = j "me" } )
            , ("meta/index.html"          , dovr { upid    = j "index" } )
            , ("content/lagrangians.ipynb", dovr { upid    = j "they_see_me_flowing"
                                                 , date    = j "2019-01-01" -- FIXME
                                                 , title   = j "They see me flowin' they hatin'"
                                                 , summary = j "Visualising some unconventional Lagrangians and their Hamiltonian flows."
                                                 })
            , ("content/grasp.md"     , dovr { upid    = j "org_grasp"
                                             , summary = j "How to capture information from your browser and stay sane"})
            , ("content/sleep-tracking.md", dovr { upid    = j "sleep_tracking"
                                                 , summary = j "How not to do it"})
            , ("content/quantified-mind.md", dovr { upid    = j "quantified_mind"
                                                  , summary = j "TODO FIXME" })
            ] :: [(String, Overrides)] where
  dovr = defaultOverrides
  j = Just

getOverrides :: String -> Overrides
getOverrides x = fromMaybe (error $ "no overrides for " ++ x) $ lookup x overrides -- TODO maybe in that case empty? dangerous though... for disqus id

overridesCtx :: Context a
overridesCtx = Context makeItem where
  -- ok, so it's name, parameters, item
  makeItem fname a item = case ovd of
      Just x  -> return $ StringField x
      Nothing -> empty
    where
    getter fname
      | fname == "issoid"      = fmap ("isso_" ++ ) . upid
      | fname == "date"        = date
      | fname == "title"       = title
      | fname == "summary"     = summary
      | fname == "description" = summary
      | otherwise              = \_ -> Nothing -- TODO ??

    ovd = getter fname $ getOverrides $ toFilePath $ itemIdentifier item

-- upid
-- should be path independent
-- should be extension independent
-- ok, so a static map for now
-- TODO if possible, define it in post itself
-- TODO make sure they are unchanged? dump them?
-- TODO warn if some are gone too?
-- getUpid :: String -> String
-- getUpid x = fromMaybe (error $ "no UPID for " ++ x) $ lookup x path2upid



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

-- myContext :: Context PandocX
-- myContext = combineContexts pandocContext defaultContext

-- thanks to https://github.com/gibiansky/blog/blob/668a5bf7ae6815a20dd6d57c900318e34c959c13/Compilers.hs
compileWithFilter :: String -> [String] -> Item String -> Compiler (Item String)
compileWithFilter cmd args = withItemBody (unixFilter cmd args)

-- TODO hmm, that should probably be pre commit hook for content git repo?
-- wouldn't hurt to run again and check?
-- python3 ./ipynb_output_filter.py <lagr.ipynb >lagr2.ipynb 

stripPrivateTodos :: Item String -> Compiler (Item String)
stripPrivateTodos = compileWithFilter "grep" ["-v", "TODO P "]

ipynbFilterOutput :: Item String -> Compiler (Item String)
ipynbFilterOutput = compileWithFilter cmd args
  where cmd = "python3"
        args = [ "/L/Dropbox/repos/ipynb_output_filter/ipynb_output_filter.py" ]


-- ugh, images do not really work in markdown..
-- , "--to", "markdown"
-- TODO patch notebook and add %matplotlib inline automalically?
ipynbRun :: Item String -> Compiler (Item String)
ipynbRun = compileWithFilter command arguments
  where command = "jupyter"
        arguments = ["nbconvert"
                    , "--execute"
                    , "--TagRemovePreprocessor.remove_cell_tags={\"noexport\"}"
                    , "--to", "html", "--template", "basic"
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

-- TODO cleanup first??
-- TODO html --basic??
ipynbCompile = stripPrivateTodos >=> ipynbFilterOutput >=> ipynbRun
  -- ipy <- ipynbRun i  -- x <&> (renameItem (\f -> replaceExtension f ".md")) -- ipynbFilterOutput >> ipynbRun
  -- let ipy_md = renameItem (\f -> replaceExtension f ".md") ipy -- change the extension to trick pandoc...
  -- pandoc <- readPandoc ipy_md
  -- let html = writePandoc pandoc
  -- let res = renameItem (\f -> replaceExtension f ".ipynb") html 
  -- return ipy

-- TODO css for ipython notebooks? highligh python?
-- TODO release ipython stuff in a separate file so it's easy to share
-- TODO mathjax (if necessary)


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

    match (fromList ["meta/me.md"]) $ do
        route   $ gsubRoute "meta/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" myCtx
            >>= relativizeUrls

    -- TODO think how to infer date?
    match "content/*.md" $ do
        route   simpleRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

-- TODO in org mode files, date should be present
-- if it's not, complain, but the whole thing shouldn't fail!


    -- TODO make a script to check that links are reachable
    -- TODO posts/etc is lame, use top level
    -- TODO rss
    -- TODO tags would be nice...
    match "content/*.ipynb" $ do
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

    let loadPosts = loadAll ("content/*.md" .||. "content/*.ipynb")


    match "meta/index.html" $ do
        route   $ gsubRoute "meta/" (const "")
        compile $ do
            posts <- recentFirst =<< loadPosts
            let indexCtx =
                    listField "posts" postCtx (return posts)
                    <> constField "title" "Home"
                    <> myCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- TODO atom -- published, updated , I guess handle carefully so there aren't too many annoying updates
    -- TODO include latest only?
    -- TODO use proper description?
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx
            posts <- loadPosts
            renderAtom myFeedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx
            posts <- loadPosts
            renderRss myFeedConfiguration feedCtx posts

    match "templates/*" $ compile templateBodyCompiler


-- left takes precedence..
myCtx :: Context String
myCtx = overridesCtx <> defaultContext

postCtx = myCtx
