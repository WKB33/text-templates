{-|
Module      : Template
Description : Framework for creating text templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating text templates. These are text with holes that can
be filled and plugged. No parsing of the actual text is done, but the text
is broken up into `chunk`'s in between the `hole`'s. Then a fill or plug
function can be defined to replace the holes with text; see `fillHole` and
`plugHole`. 

The recommended way to programmatically generate templates is to use the
quasi-quoter which makes writing templates easier. If you don't want to depend
on Template Haskell, then we strongly recommend to use the template combinators,
because they keep track of internal state to make certain operations on
templates efficient.
-}
{-# LANGUAGE PatternSynonyms #-}
module Data.TextTemplate (-- * Templates 
                            Template
                           ,pattern Empty
                           ,pattern Chunk
                           ,pattern Compose
                           ,Hole
                            -- ** Template Combinators
                           ,hole
                           ,filled
                           ,chunk
                           ,(+>)
                           ,showAST
                           ,sepTemplatesBy 
                           ,betweenTemplate
                           ,bracketTemplate
                           ,braceTemplate
                           -- ** Template Properties
                           ,unfilledHoles
                           ,filledHoles
                           ,numberOfUnfilledHoles
                           ,numberOfFilledHoles                        
                           -- ** Plugging Holes in Templates
                           ,plugHole
                           ,plugAll
                           -- ** Equality and Matching
                           ,(==>)
                           -- ** Converting from Templates
                           ,chunkToText
                           -- * Quasi-Quoter for Templates
                           ,template
                           ,textTemplate2QExp
                           ,template2QExp
                           -- * Text Combinators
                           ,between
                           ,braces
                           ,brackets
                           ,prettyList
                           ,prettyDouble
                           ,doubleQuote
                           -- * Combining Templates with Other Types
                           ,TU(..)
                           ,parseTU
                           ) where

import Data.TextTemplate.Text
import Data.TextTemplate.TemplateInternal
import Data.TextTemplate.QQInternal
