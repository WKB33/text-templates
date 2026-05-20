{-|
Module      : Template
Description : Framework for creating string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating string templates. These are strings with holes that can
be plugged. No parsing of the actual string is done, but the string is broken up
into `chunk`'s in between the `hole`'s. Then a plug function can be defined to
replace the holes with strings; see `plug`. 

The recommended way to programmatically generate templates is to use the
quasi-quoter which makes writing templates easier. If you don't want to depend
on Template Haskell, then you can also use the template combinators.
-}
module Data.StringTemplate (-- * Templates 
                            Template
                            -- ** Template Combinators
                           ,hole
                           ,chunk
                           ,(+>)
                           ,showAST
                           -- ** Plugging Holes in Templates
                           ,plug
                           -- ** Equality and Matching
                           ,(==>)
                           ,match
                           -- * Quasi-Quoter for Templates
                           ,template
                           ,stringTemplate2QExp
                           ) where

import Data.StringTemplate.TemplateInternal
import Data.StringTemplate.QQInternal
