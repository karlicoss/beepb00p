{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Common where

import Hakyll (Item, Compiler, Context, unixFilter, withItemBody)

(|>) = flip ($)
(|.) = flip (.)

the :: [a] -> a
the [x] = x
the l   = error $ "expected single item, got " ++ (show $ length l) ++ " instead"


-- thanks to https://github.com/gibiansky/blog/blob/668a5bf7ae6815a20dd6d57c900318e34c959c13/Compilers.hs
compileWithFilter :: String -> [String] -> Item String -> Compiler (Item String)
compileWithFilter cmd args = withItemBody (unixFilter cmd args)
