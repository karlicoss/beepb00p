--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Debug.Trace (trace)

import           Hakyll

import   Control.Applicative (empty, (<|>))
import         Control.Monad ((>=>), filterM)
import       Data.List.Split (splitOn)
import            Data.Maybe (fromJust, fromMaybe, catMaybes, isJust, isNothing)
import           Data.Monoid (mappend)
import    System.Environment (lookupEnv)
import      System.IO.Unsafe (unsafePerformIO)

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


-- TODO shit this is problematic for all simple web servers, they think it's octet-stream :(
chopOffRoute thing = gsubRoute thing (const "" )

-- TODO fucking hell it's annoying. couldn't force github pages or preview server to support that
html = setExtension "html"

(|-) = composeRoutes


data Thing a = Thing (Context a) (Compiler a) [Dependency]

main :: IO ()
main = do
  hakyll $ do
    match ("meta/robot-face.png" .||. "meta/robots.txt") $ do
        route   $ chopOffRoute "meta/"
        compile   copyFileCompiler

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        -- compile compressCssCompiler -- compressed css is pretty git unfriendly. I bet it doesn't matter in modern browsers
        compile copyFileCompiler

    match "content/**.jpg" $ do
      route   $ chopOffRoute "content/"
      compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

    -- TODO when I publish that, mention that this thing is important!

    -- TODO ugh. use mapM/map (value $) or something?
    let source pat = match pat $ compile getResourceBody

    source "misc/compile-org"
    source "misc/compile-org.el"
    compileOrgBin   <- makePatternDependency $ "misc/compile-org" .||. "misc/compile-org.el"

    source "misc/compile-ipynb"
    source "misc/mybasic.tpl"
    source "misc/ipynbconfig.py"
    compileIpynbBin <- makePatternDependency $ "misc/compile-ipynb" .||. "misc/mybasic.tpl" .||. "misc/ipynbconfig.py"

    let doMeta pat ectx comp deps = rulesExtraDependencies deps $ match pat $ do
          let ctx = special <> ectx
          route   $ chopOffRoute "content/meta/" |- html
          compile $ comp
            >>= postCompiler ctx

    let doSpecial pat ectx comp deps = rulesExtraDependencies deps $ match pat $ do
          let ctx = special <> ectx
          route   $ chopOffRoute "content/special/" |- html
          compile $ comp
              >>= postCompiler ctx

    let doGenerated pat ctx comp deps = rulesExtraDependencies deps $ match pat $ do
          route   $ chopOffRoute "content/generated/" |- html
          compile $ comp
             >>= postCompiler ctx

    let doPost pat ctx comp deps = rulesExtraDependencies deps $ match pat $ do
          route   $ chopOffRoute "content/" |- html
          compile $ comp
             >>= postCompiler ctx

    let draftCtx = constField "date" "[2019-09-20]" -- arbitrary
                <> constField "upid" "whaat"
                <> constField "issoid" "whaat"
                -- <> field "upid" (\x -> itemIdentifier x |> show |> return) -- TODO eh, should depend on item name maybe?'
    let doDraft pat ctx  = doPost pat (ctx <> draftCtx)

    let doAll dodo prefix = do
          dodo (globalFilter .&&. fromGlob (prefix ++ ".org"))   orgCtx   orgCompiler    [compileOrgBin]
          dodo (globalFilter .&&. fromGlob (prefix ++ ".ipynb")) ipynbCtx ipynbCompiler  [compileIpynbBin]
          dodo (globalFilter .&&. fromGlob (prefix ++ ".md"))    mdCtx    pandocCompiler []

    -- TODO publish: doAll is good for handling different formats

    doAll doMeta      "content/meta/**"
    doAll doSpecial   "content/special/**"
    doAll doPost      "content/*"
    doAll doDraft     "content/drafts/*"
    doAll doGenerated "content/generated/*"


-- TODO in org mode files, date should be present
-- if it's not, complain, but the whole thing shouldn't fail!

    -- TODO think how to infer date?

    -- TODO posts/etc is lame, use top level
    -- TODO tags would be nice...
    -- TODO perhaps need to use snapshot for caching??

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

    match "meta/index.html" $ do
        route   $ gsubRoute "meta/" (const "")
        compile $ do
            -- TODO sorting: I guess we want datetime in case of multiple posts on the same day
            -- TODO not sure if should reuse same filterM thing I'm using for feed?..
            forIndex <- recentFirst =<< loadAll patterns
            let indexCtx =
                    listField "posts" postCtx (return forIndex)
                    <> constField "title" "Home"
                    <> baseCtx

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
    let feedPosts = recentFirst =<< loadAll patterns
    let feedCtx = postCtx <> bodyField "description"

    let createFeed file render = create [file] $ do
          route idRoute
          compile $ do
            posts <- feedPosts
            public <- filterM (\x -> isNothing <$> getMetadataField (itemIdentifier x) "draft") posts
            render myFeedConfiguration feedCtx public

    createFeed "atom.xml" renderAtom
    createFeed "rss.xml"  renderRss

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

tagExtractor :: DependentField
tagExtractor reader = do
  (StringField filetags) <- reader "filetags"
  let tags = filter (\x -> x /= "") $ splitOn ":" filetags
  return $ ListField baseCtx $ map toItem tags


orgTagsCtx :: Context a -> Context a
orgTagsCtx ctx = (dependentField "tags" tagExtractor ctx) <> listField "tags" baseCtx (return []) <> ctx

dateCtx :: Context a -> Context a
dateCtx ctx = (dependentField "date" dateExtractor ctx)
           <> ctx
           -- <> (dependentField "published" dateExtractor ctx)
           -- <> (dependentField "updated" dateExtractor ctx)
-- TODO ugh. not sure if I want 'updated' to be curated or not?? basically it should reflect underlying content changes only, no css etc
-- TODO for symlinks, should resolve them as well
-- hmm, for updated git wouldn't preserve timestamp; so I should use last git modification time if it's in git, or follow symlink and use mtime? What if it's in git there?.... need some debugging overview

-----

globalFilter :: Pattern
globalFilter = fromGlob pat where
  filt = unsafePerformIO $ lookupEnv "FILTER"
  pat = fromMaybe "**" filt



stableCtx :: Context String
stableCtx = if isStable then constField "is_stable" "flag" else mempty where
  isStable = isJust $ unsafePerformIO $ lookupEnv "BEEPB00P_STABLE"


baseCtx = stableCtx <> defaultContext

metaTags = listContextWith "tags"

postCtx :: Context String
-- orgMetas need to be sort of part of postCtx so its fields are accessible for index page, RSS, etc
-- TODO def need to write about it, this looks like the way to go and pretty tricky
-- left takes precedence
postCtx = dateCtx $ issoIdCtx $ metaTags <> (orgTagsCtx orgMetas) <> stableCtx <> baseCtx


-- TODO ok, so this works.. I wonder if I should rely on yaml list or split by spaces instead... later is more org mode friendly. or could have a special org mode context??
listContextWith :: String -> Context a
listContextWith s = listFieldWith s baseCtx (getList s)


-- thanks to https://ohanhi.com/from-jekyll-to-hakyll.html for initial inspiration,
-- however getUndelying would return index.html for index page
-- thanks to this post http://beerendlauwers.be/posts/2015-08-17-hacking-on-hakyll-pt-2.html for listFieldWith

getList :: String -> Item a -> Compiler [Item String]
getList s item = do
    let idd = itemIdentifier item
    meta <- getMetadata idd
    let list =  meta |> lookupStringList s
    case list of
      Nothing -> fail $ "No " ++ s ++ " field"
      Just ts -> map toItem ts |> return


toItem x =
    Item (fromFilePath x) x
