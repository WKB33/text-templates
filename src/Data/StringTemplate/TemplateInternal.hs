{-|
Module      : Template
Description : Framework for creating string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating string templates. These are strings with holes that 
can be plugged. No parsing of the actual string is done, but the string is 
broken up into `chunk`'s in between the `hole`'s. Then a plug function can 
be defined to replace the holes with strings; see `plug`.
-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE RankNTypes                   #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE BangPatterns                 #-}
{-# LANGUAGE TypeAbstractions             #-}
module Data.StringTemplate.TemplateInternal where

import GHC.TypeNats            (type (+)
                               ,Natural
                               ,Nat)
import Data.Type.Natural       (S)
import Data.Text               qualified as DT
import Data.Constraint         (Dict (..)
                               ,(\\))
import Data.Constraint.Nat     (plusAssociates
                               ,plusCommutes)
import Text.Regex.TDFA         ((=~))

-- | A internal template with `n` holes. 
data ITemplate (n :: Nat) where
    Chunk   :: DT.Text -> ITemplate 0
    Compose :: DT.Text -> Natural -> ITemplate n -> ITemplate (S n)

instance Show (ITemplate n) where
    show :: ITemplate n -> String    
    show (Chunk t)       = DT.unpack t
    show (Compose prefix h rest) = DT.unpack prefix <> "${" <> show h <> "}" <> show rest

-- | A template with pluggable holes. We do not expose the underlying
-- constructors in favor of the combinators.
data Template where
    Template :: ITemplate m -> Template

instance Show Template where
    show :: Template -> String
    show (Template t) = show t

-- | Equality of ITemplates.
(>==>) :: forall m n.
          ITemplate m
       -> ITemplate n
       -> Bool
(Chunk chk1)        >==> (Chunk chk2)        = chk1 == chk2
(Compose chk1 _ r1) >==> (Compose chk2 _ r2) = (chk1 == chk2 && r1 >==> r2) 
_                   >==> _                   = False

instance Eq Template where
    (==) :: Template -> Template -> Bool
    (==) = (==>)

-- | Equality of templates. Two templates are considered equivalent if and only
-- if they differ by hole labels only.
(==>) :: Template
      -> Template
      -> Bool
(Template t1) ==> (Template t2) = t1 >==> t2

-- | Composition of ITemplates.
(>+>) :: forall m n.
         ITemplate m 
      -> ITemplate n 
      -> ITemplate (m+n)
(Chunk chk1)        >+> (Chunk chk2)    = Chunk $ chk1 <> chk2
(Chunk chk)         >+> (Compose p h r) = Compose (chk <> p) h r
(Compose @n1 p h r) >+> t               = (Compose p h $ r >+> t) \\ p1 @m @n1 @n
    where
        p1 :: forall m n1 n . m ~ (n1 + 1) => Dict (((n1 + n) + 1) ~ (m + n))
        p1 = Dict \\ plusCommutes @n1 @n \\ plusAssociates @n @n1 @1 \\ plusCommutes @n @m

-- | Composition of templates.
(+>) :: Template
     -> Template
     -> Template
(Template t1) +> (Template t2) = Template $ t1 >+> t2

-- | A hole.
hole :: Natural
     -> Template
hole i = Template $ Compose "" i (Chunk "")

-- | A chunk is a substring to a larger string.
chunk :: DT.Text -- ^ Substring.
      -> Template
chunk = Template . Chunk

-- | Convert a template into a `Text`, but in AST form rather than pretty
-- printing. The `Show` instance for `Template` is set to pretty print, but for
-- debugging it is sometimes useful to see the raw AST.
showAST :: Template -> DT.Text
showAST (Template (Chunk x))       = "Chunk "   <> (DT.show x)
showAST (Template (Compose p h r)) = "Compose " <> (DT.show p) <> " " <> (DT.show h) <> " (" <> (showAST (Template r)) <> ")"

-- | Plugs every hole in a template using the given plug function. If the plug
-- function is defined for every hole in the input template, then this function
-- guarantees a template with no holes (a text) is returned.
plug :: Template                     -- ^ Template to plug
     -> (Natural -> Maybe DT.Text)   -- ^ Plug function
     -> Maybe DT.Text
plug (Template t) f = 
    case _plug f t of        
        Just (Chunk c) -> Just c
        _ -> Nothing
    where
        -- | Main logic for plug.
        _plug 
            :: (Natural -> Maybe DT.Text) -- ^ Plug function.
            -> ITemplate n                -- ^ ITemplate to plug.
            -> Maybe (ITemplate 0)
        _plug f (Compose chk h r) = do
            chk' <- f h
            Chunk chk'' <- _plug f r
            return . Chunk $ chk <> chk' <> chk''
        _plug _ t@(Chunk _) = Just t

-- | Convert a template into a regular expression. This is used to match against
-- a template.
toRegex :: Template -> DT.Text
toRegex (Template t) = _toRegex t
    where
        _toRegex :: ITemplate n -> DT.Text
        _toRegex (Chunk chk)       = chk
        _toRegex (Compose chk _ r) = chk <> ".*" <> _toRegex r

-- | Match a string against a template. Outputs @True@ when the entire string
-- matches the template where all its holes are plugged with the regular
-- expression @.*@.
match :: Template -- ^ Template to be matched on
      -> DT.Text  -- ^ String to match against
      -> Bool
match t s = s =~ regex
    where
        regex = toRegex t
