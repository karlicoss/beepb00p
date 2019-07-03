--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import           Data.Monoid (mappend)
import           Hakyll

import Control.Applicative (empty)
import Control.Monad ((>=>))

import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import           Data.Time.Clock   (UTCTime (..))
import qualified Data.Time.Format  as TF

import System.FilePath (takeExtension, replaceExtension, (</>))

import Text.Pandoc (readOrg, Pandoc(..), docTitle, docDate, Meta, Inline)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Options (def, writerVariables, writerTableOfContents)
import           Control.Applicative ((<|>))

import Hakyll.Web.Pandoc
import Hakyll.Web.Tags (buildTags)
import Debug.Trace (trace)

import qualified Hakyll.Core.Store       as Store



(|>) = flip ($)
(|.) = flip (.)

the :: [a] -> a
the [x] = x
the l   = error $ "expected single item, got " ++ (show $ length l) ++ " instead"

baseUrl = "https://beepb00p.xyz"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Mildly entertaining"
    , feedAuthorName  = "karlicoss"
    , feedAuthorEmail = "karlicoss@gmail.com"
    , feedDescription = "feed" -- TODO ?
    , feedRoot        = baseUrl -- TODO how to test feed?
    }


-- , ("content/lagrangians.ipynb"     , dovr { upid    = j "they_see_me_flowing"
--                                           , date    = j "2019-01-01" -- FIXME
--                                           , title   = j "They see me flowin' they hatin'"
--                                           , summary = j "Visualising some unconventional Lagrangians and their Hamiltonian flows."


-- upid
-- should be path independent
-- should be extension independent
-- ok, so a static map for now
-- TODO make sure they are unchanged? dump them?
-- TODO warn if some are gone too?


-- pandocMeta :: (Meta -> [Inline]) -> (Item Pandoc -> Compiler String)
-- pandocMeta extractor Item {itemBody=Pandoc meta _} = return $ stringify $ extractor meta -- TODO proper html??

-- -- TODO extract that stuff somewhere and share??
-- orgFileTags = field "filetags" (\p -> return "TODO FILETAGS")
-- orgAuthor = constField "author" "Dima" -- TODO docAuthors??
-- orgTitle = field "title" $ pandocMeta docTitle
-- orgDate = field "date" $ pandocMeta docDate

-- pandocContext :: Context Pandoc
-- pandocContext = orgFileTags <> orgAuthor <> orgTitle <> orgDate

-- -- TODO ugh. surely it can't be that ugly right?
-- data PandocX = PandocX Pandoc String

-- combineItems :: (a -> b -> c) -> Item a -> Item b -> Item c
-- combineItems f Item{itemBody=ba, itemIdentifier=ii} Item{itemBody=bb} = Item {itemBody=f ba bb, itemIdentifier=ii}

-- combineContexts :: Context Pandoc -> Context String -> Context PandocX
-- combineContexts (Context f) (Context g) = Context $ \k a Item{itemBody=PandocX pdoc rendered} -> f k a Item {itemBody=pdoc, itemIdentifier=""} <|> g k a Item {itemBody=rendered, itemIdentifier=""} -- TODO break down item ;

-- TODO readPandocWith??

-- myContext :: Context PandocX
-- myContext = combineContexts pandocContext defaultContext

----- ipython stuff

-- thanks to https://github.com/gibiansky/blog/blob/668a5bf7ae6815a20dd6d57c900318e34c959c13/Compilers.hs
compileWithFilter :: String -> [String] -> Item String -> Compiler (Item String)
compileWithFilter cmd args = withItemBody (unixFilter cmd args)

-- TODO hmm, that should probably be pre commit hook for content git repo?
-- wouldn't hurt to run again and check?
-- python3 ./ipynb_output_filter.py <lagr.ipynb >lagr2.ipynb 

stripPrivateTodos :: Item String -> Compiler (Item String)
stripPrivateTodos = compileWithFilter "grep" ["-v", "NOEXPORT"]

ipynbFilterOutput :: Item String -> Compiler (Item String)
ipynbFilterOutput = compileWithFilter cmd args
  where cmd = "python3"
        args = [ "/L/soft/ipynb_output_filter/ipynb_output_filter.py" ] -- TODO use nbstripout??


-- ugh, images do not really work in markdown..
-- , "--to", "markdown"
-- TODO patch notebook and add %matplotlib inline automalically?
ipynbRun :: Item String -> Compiler (Item String)
ipynbRun = compileWithFilter command arguments
  where command = "jupyter"
        arguments = ["nbconvert"
                    , "--execute"
                    , "--TagRemovePreprocessor.remove_cell_tags={\"noexport\"}" -- TODO emacs tags?
                    , "--to", "html", "--template", "misc/mybasic.tpl"
                    , "--stdin"
                    , "--stdout"
                    ]

-- renameItem :: (String -> String) -> Item a -> Item a
-- renameItem f i =  i { itemIdentifier = new_id } where
--   old_id = itemIdentifier i
--   old_version = identifierVersion old_id
--   new_id = setVersion old_version $ (fromFilePath $ f $ toFilePath old_id)

-- TODO ugh itemIdentifier is not exported???
-- renameItem f x = x { itemIdentifier = new_id } where
--   old_id = itemIdentifier x -- TODO do I need lens?..
--   old_path = identifierPath old_id
--   new_path = f old_path
--   new_id = old_id { identifierPath = new_path }

ipynbCompile = stripPrivateTodos >=> ipynbFilterOutput >=> ipynbRun
  -- ipy <- ipynbRun i  -- x <&> (renameItem (\f -> replaceExtension f ".md")) -- ipynbFilterOutput >> ipynbRun
  -- let ipy_md = renameItem (\f -> replaceExtension f ".md") ipy -- change the extension to trick pandoc...
  -- pandoc <- readPandoc ipy_md
  -- let html = writePandoc pandoc
  -- let res = renameItem (\f -> replaceExtension f ".ipynb") html 
  -- return ipy

-- TODO release ipython stuff in a separate file so it's easy to share

----- end of ipython stuff


---- start of org mode stuff

-- pandoc doesn't seem to be capable of handling many org clases.. 
-- https://github.com/jgm/pandoc/blob/f3080c0c22470e7ecccbef86c1cd0b1339a6de9b/src/Text/Pandoc/Readers/Org/ExportSettings.hs#L61
renderOrg :: Item String -> Compiler (Item String)
renderOrg   = compileWithFilter "misc/compile-org" []

extractBody = compileWithFilter "xmllint" ["--html", "--xpath", "//body/node()", "--format", "-"]

orgCompile = renderOrg >=> extractBody

raw_org_key = "raw_org"
meta_start = "#+"
meta_sep   = ": "

type OrgMetas = [(String, String)]
type OrgBody = String

-- TODO ugh. very hacky...
orgMetadatas :: OrgBody -> OrgMetas
orgMetadatas = lines |. map tryMeta |. catMaybes
  where
    tryMeta :: String -> Maybe (String, String)
  -- TODO catMaybe?
    tryMeta line = do
      -- TODO ugh. a bit ugly...
      let split = splitOn meta_start line
      case split of
        ("": rem) ->
           let split2 = splitOn meta_sep $ concat rem in
             case split2 of
               -- we intercalate here since colons could be in title
               -- TODO ugh. perhaps should have used regex instead
               (fieldname: rem2) -> Just (fieldname |> map toLower, intercalate meta_sep rem2)
               _ -> Nothing
        _ -> Nothing

orgMetas :: Context String
orgMetas = Context $ \key _ item -> do
  let idd = itemIdentifier item
  let path = toFilePath idd
  if takeExtension path /= ".org" then empty else do
    raw_org :: Item String  <- loadSnapshot idd raw_org_key
    let metas = orgMetadatas $ itemBody raw_org
    let meta = lookup key metas
    maybe empty (StringField |. return) meta

--- end of org mode stuff

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

-- TODO that's pretty horrible... maybe I need a special item type... and combine compilers?
orgCompiler   = do
  res <- getResourceString
  _ <- saveSnapshot raw_org_key res
  orgCompile res
ipynbCompiler = getResourceString >>= ipynbCompile
-- TODO careful not to pick this file up when we have more org posts
-- perhaps should just move the link out of content root


-- -- let ff  = constField "css" "hello"
-- let ff = field "css" $ \x -> itemBody x
-- let ctx = postCtx <> listField "extra_styles" ff (return ["HELLO", "WHOOPS"])

main :: IO ()
main = hakyll $ do
    match ("meta/favicon.ico" .||. "meta/robots.txt") $ do
        route   $ gsubRoute "meta/" (const "")
        compile   copyFileCompiler


    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        -- compile compressCssCompiler -- compressed css is pretty git unfriendly. I bet it doesn't matter in modern browsers
        compile copyFileCompiler


    -- TODO shit this is problematic for all simple web servers, they think it's octet-stream :(
    let chopOffRoute thing = gsubRoute thing (const "" )
          `composeRoutes` setExtension "html" -- TODO fucking hell it's annoying. couldn't force github pages or preview server to support that

    let postRoute = chopOffRoute "content/"

    match (fromList ["meta/me.md", "meta/feed.md"]) $ do
        route   $ gsubRoute "meta/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" postCtx -- TODO mdCtx?
            >>= relativizeUrls

    match "content/meta/*.org" $ do
        let ctx = special <> orgCtx
        route   $ chopOffRoute "content/meta/"
        compile $ orgCompiler
            >>= postCompiler ctx

    match "content/special/*.org" $ do
        let ctx = special <> orgCtx
        route   $ chopOffRoute "content/special/"
        compile $ orgCompiler
            >>= postCompiler ctx

    -- TODO think how to infer date?
    match "content/*.md" $ do
        let ctx = mdCtx
        route   postRoute
        compile $ pandocCompiler
            >>= postCompiler ctx

-- TODO in org mode files, date should be present
-- if it's not, complain, but the whole thing shouldn't fail!

    -- TODO make a script to check that links are reachable
    -- TODO posts/etc is lame, use top level
    -- TODO tags would be nice...
    match "content/*.ipynb" $ do
        let ctx = ipynbCtx
        route   postRoute
        compile $ ipynbCompiler
              >>= postCompiler ctx

    -- TODO perhaps need to use snapshot for caching??
    match "content/*.org" $ do
        let ctx = orgCtx
        route   postRoute
        compile $ orgCompiler
              >>= postCompiler ctx

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

    -- let loadPosts = loadAll ("content/*.md" .||. "content/*.ipynb")
    let patterns = "content/*.md" .||. "content/*.ipynb" .||. "content/*.org"

    match "meta/index.html" $ do
        route   $ gsubRoute "meta/" (const "")
        compile $ do
            -- TODO sorting: I guess we want datetime in case of multiple posts on the same day
            posts <- recentFirst =<< loadAll patterns
            let indexCtx =
                    listField "posts" postCtx (return posts)
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
    let feedCtx = postCtx <> bodyField "description"
    -- https://jip.dev/posts/post-feed-in-hakyll/
    -- let feedPosts = loadAllSnapshots patterns "feed-body" -- TODO err.. what's up with that, why is it not used???
    let feedPosts = loadAll patterns

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- feedPosts
            renderAtom myFeedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- feedPosts
            renderRss myFeedConfiguration feedCtx posts

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
dateCtx ctx = (dependentField "date" dateExtractor ctx) <> ctx

-----


postCtx :: Context String
-- orgMetas need to be sort of part of postCtx so its fields are accessible for index page, RSS, etc
-- TODO def need to write about it, this looks like the way to go and pretty tricky
postCtx = dateCtx $ issoIdCtx $ listContextWith "tags" <> orgMetas <> defaultContext

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
