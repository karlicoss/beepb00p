--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Debug.Trace (trace)

import           Hakyll

import   Control.Applicative (empty, (<|>))
import         Control.Monad ((>=>))
import            Data.Maybe (fromJust, fromMaybe, catMaybes)
import           Data.Monoid (mappend)

import           Data.Time.Clock   (UTCTime (..))
import qualified Data.Time.Format  as TF

import Hakyll.Web.Tags (buildTags)
import qualified Hakyll.Core.Store       as Store


import Common ((|>), (|.), compileWithFilter, the)
import Org
import Ipynb


baseUrl = "https://beepb00p.xyz"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Mildly entertaining"
    , feedAuthorName  = "karlicoss"
    , feedAuthorEmail = "karlicoss@gmail.com"
    , feedDescription = "feed" -- TODO ?
    , feedRoot        = baseUrl -- TODO how to test feed?
    }

-- upid
-- should be path independent
-- should be extension independent
-- ok, so a static map for now
-- TODO make sure they are unchanged? dump them?
-- TODO warn if some are gone too?


postCompiler ctx =
    saveSnapshot "feed-body"
    >=> loadAndApplyTemplate "templates/post.html" ctx
    >=> loadAndApplyTemplate "templates/default.html" ctx
    >=> relativizeUrls

style x = constField ("style_" ++ x) "x"  -- kinda like a flag
-- TODO dispatch based on extension?

mdCtx    = style "md"    <> postCtx
orgCtx   = style "org"   <> postCtx
ipynbCtx = style "ipynb" <> postCtx

special = constField "type_special" "x"


-- -- let ff  = constField "css" "hello"
-- let ff = field "css" $ \x -> itemBody x
-- let ctx = postCtx <> listField "extra_styles" ff (return ["HELLO", "WHOOPS"])

main :: IO ()
main = hakyll $ do
    -- TODO shit this is problematic for all simple web servers, they think it's octet-stream :(
    let chopOffRoute thing = gsubRoute thing (const "" )

    -- TODO just keep it in 'content' to simplify?
    match ("meta/favicon.ico" .||. "meta/robots.txt") $ do
        route   $ chopOffRoute "meta/"
        compile   copyFileCompiler


    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        -- compile compressCssCompiler -- compressed css is pretty git unfriendly. I bet it doesn't matter in modern browsers
        compile copyFileCompiler


    -- TODO fucking hell it's annoying. couldn't force github pages or preview server to support that
    let html = setExtension "html"

    let (|-) = composeRoutes


    match (fromList ["meta/me.md", "meta/feed.md"]) $ do
        route   $ chopOffRoute "meta/" |- html
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" postCtx -- TODO mdCtx?
            >>= relativizeUrls

    match "content/**.jpg" $ do
      route   $ chopOffRoute "content/"
      compile copyFileCompiler


    match "content/meta/*.org" $ do
        let ctx = special <> orgCtx
        route   $ chopOffRoute "content/meta/" |- html
        compile $ orgCompiler
            >>= postCompiler ctx

    let doSpecial pat ectx comp = match pat $ do
          let ctx = special <> ectx
          route   $ chopOffRoute "content/special/" |- html
          compile $ comp
              >>= postCompiler ctx

    doSpecial "content/special/*.org"   orgCtx   orgCompiler
    doSpecial "content/special/*.ipynb" ipynbCtx ipynbCompiler

    let doPost pat ctx comp = match pat $ do
          route   $ chopOffRoute "content/" |- html
          compile $ comp
             >>= postCompiler ctx


    -- TODO fixme find out how to combine patterns
    match "content/generated/*.md" $ do
        let ctx = mdCtx
        route   $ chopOffRoute "content/generated/" |- html
        compile $ pandocCompiler
            >>= postCompiler ctx

-- TODO in org mode files, date should be present
-- if it's not, complain, but the whole thing shouldn't fail!

    -- TODO think how to infer date?

    -- TODO make a script to check that links are reachable
    -- TODO posts/etc is lame, use top level
    -- TODO tags would be nice...
    -- TODO perhaps need to use snapshot for caching??
    doPost "content/*.md"    mdCtx    pandocCompiler
    doPost "content/*.ipynb" ipynbCtx ipynbCompiler
    doPost "content/*.org"   orgCtx   orgCompiler

-- TODO appendIndex??https://github.com/aherrmann/jekyll_style_urls_with_hakyll_examples/blob/master/site.hs
-- https://github.com/turboMaCk/turboMaCk.github.io/blob/develop/site.hs#L61 ??
-- TODO reference to how to read my posts?? e.g. what todo states mean etc

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

    let patterns =
               "content/*.md"
          .||. "content/generated/*.md"
          .||. "content/*.ipynb"
          .||. "content/*.org"

    tags <- buildTags patterns (fromCapture "tags/*.html")
    -- TODO ok, so tags are in here
    -- TODO how to refer to them on tags page?
    -- TODO that would require some elaborate matching with CUSTOM_ID...

    -- tagsRules tags $ \tag pattern -> do
    --   let title = "Posts tagged \"" ++ tag ++ "\""
    --   route idRoute
    --   compile $ do
    --       posts <- recentFirst =<< loadAll pattern
    --       let ctx = constField "title" title
    --                 `mappend` listField "posts" postCtx (return posts)
    --                 `mappend` defaultContext

    --       makeItem ""
    --           >>= loadAndApplyTemplate "templates/tag.html" ctx
    --           >>= relativizeUrls

    let prefilter p = True -- itemIdentifier p /= "content/annotating.org" -- TODO FIXME meh, implement properly..

    match "meta/index.html" $ do
        route   $ gsubRoute "meta/" (const "")
        compile $ do
            -- TODO sorting: I guess we want datetime in case of multiple posts on the same day
            posts <- recentFirst =<< loadAll patterns
            let forIndex = filter prefilter posts
            -- TODO eh, extract it...
            let indexCtx =
                    listField "posts" postCtx (return forIndex)
                    <> constField "title" "Home"
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- TODO atom -- published, updated , I guess handle carefully so there aren't too many annoying updates
    -- TODO include latest only?
    -- TODO not sure if need to prettify description
    -- TODO use 'content' field??


    -- https://jip.dev/posts/post-feed-in-hakyll/
    -- let feedPosts = loadAllSnapshots patterns "feed-body" -- TODO err.. what's up with that, why is it not used???
    let feedPosts = loadAll patterns
    let feedCtx = postCtx <> bodyField "description"

    let createFeed file render = create [file] $ do
          route idRoute
          compile $ do
            posts <- feedPosts
            let forIndex = filter prefilter posts
            render myFeedConfiguration feedCtx forIndex

    createFeed "atom.xml" renderAtom
    createFeed "rss.xml"  renderRss

    match "templates/*" $ compile templateBodyCompiler

type DependentField = (String -> Compiler ContextField) -> Compiler ContextField

issoid :: DependentField
issoid handle = do
  (StringField value) <- handle "upid"
  return $ StringField $ "isso_" ++ value

-- TODO ugh, that looks somewhat wrong and against the Monoid interface, but not sure if I can do anything in the absence of metadata
dependentField :: String -> DependentField -> Context a -> Context a
dependentField new_key value (Context c) = Context $ \key a item -> do
   if key /= new_key
     then empty
     else value (\k -> c k a item)


-----
issoIdCtx :: Context a -> Context a
issoIdCtx ctx = (dependentField "issoid" issoid ctx) <> ctx

-- issoIdCtx = field "issoid" $ \item -> do
--   let idd = itemIdentifier item
--   meta <- getMetadata idd -- TODO ugh. wonder if I need to rewrite back to getting it from Context so org compiling works
--   meta |> lookupString "upid" |> fromJust |> ("isso_" ++ ) |> return
-----


-----
-- judging by https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Template.Context.html#dateFieldWith,
-- builtin dateField always looks at item UTC time, which is not necessarily what we want
-- also it suffers from the same problem of depending on intrinsic metadata :(
inputDtFormats =
    [ "%a, %d %b %Y %H:%M:%S %Z"
    , "%Y-%m-%dT%H:%M:%S%Z"
    , "%Y-%m-%d %H:%M:%S%Z"
    , "%Y-%m-%d"
    , "%B %e, %Y"

    -- org mode timestamps
    , "[%Y-%m-%d %a %H:%M]"
    , "[%Y-%m-%d %a]"
    ]
outputDtFormat = "%d %B %Y" -- TODO might want to include time in some places...

dateExtractor :: DependentField
dateExtractor reader = do
  (StringField dstr) <- reader "date"
  let ut :: UTCTime = the $ catMaybes $ [TF.parseTimeM False TF.defaultTimeLocale fmt dstr | fmt <- inputDtFormats]
  let ds = TF.formatTime TF.defaultTimeLocale outputDtFormat ut
  return $ StringField $ ds

dateCtx :: Context a -> Context a
dateCtx ctx = (dependentField "date" dateExtractor ctx)
           <> ctx
           -- <> (dependentField "published" dateExtractor ctx)
           -- <> (dependentField "updated" dateExtractor ctx)
-- TODO ugh. not sure if I want 'updated' to be curated or not?? basically it should reflect underlying content changes only, no css etc
-- TODO for symlinks, should resolve them as well
-- hmm, for updated git wouldn't preserve timestamp; so I should use last git modification time if it's in git, or follow symlink and use mtime? What if it's in git there?.... need some debugging overview

-----


postCtx :: Context String
-- orgMetas need to be sort of part of postCtx so its fields are accessible for index page, RSS, etc
-- TODO def need to write about it, this looks like the way to go and pretty tricky
postCtx = dateCtx $ issoIdCtx $ listContextWith "tags" <> orgMetas <> defaultContext
-- TODO need to handle org tags via dependentCtx as well.. not sure perhaps merge with ones in metadata since org syntax doesn't allow some of the tags?

-- TODO ok, so this works.. I wonder if I should rely on yaml list or split by spaces instead... later is more org mode friendly. or could have a special org mode context??
listContextWith :: String -> Context a
listContextWith s = listFieldWith s defaultContext (getList s)


-- thanks to https://ohanhi.com/from-jekyll-to-hakyll.html for initial inspiration,
-- however getUndelying would return index.html for index page
-- thanks to this post http://beerendlauwers.be/posts/2015-08-17-hacking-on-hakyll-pt-2.html for listFieldWith

getList :: String -> Item a -> Compiler [Item String]
getList s item = do
    let idd = itemIdentifier item
    meta <- getMetadata idd
    meta
        |> lookupStringList s
        |> fromMaybe [] -- TODO not sure, maybe fromJust makes more sense?
        |> map toItem
        |> return


toItem x =
    Item (fromFilePath x) x
