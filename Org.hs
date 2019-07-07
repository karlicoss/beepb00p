{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Org where

import Control.Applicative (empty)
import Control.Monad ((>=>))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import System.FilePath (takeExtension, replaceExtension, (</>))


import Hakyll (Item, Compiler, Context, Context(..), ContextField(StringField), loadSnapshot, itemBody, itemIdentifier)

import Common (compileWithFilter, (|>), (|.))

-- import Text.Pandoc (readOrg, Pandoc(..), docTitle, docDate, Meta, Inline)
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
  let path = show idd
  if takeExtension path /= ".org" then empty else do
    raw_org :: Item String  <- loadSnapshot idd raw_org_key
    let metas = orgMetadatas $ itemBody raw_org
    let meta = lookup key metas
    maybe empty (StringField |. return) meta

--- end of org mode stuff
