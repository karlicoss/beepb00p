{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Ipynb where

import Control.Monad ((>=>))

import Hakyll (Item, Compiler, Context, Context(..), ContextField(StringField), getResourceString, itemIdentifier)

import Common (compileWithFilter, (|>), (|.))



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

ipynbCompiler = getResourceString >>= ipynbCompile