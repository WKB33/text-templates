{-|
Module      : Template
Description : Framework for creating string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating string templates. These are strings with holes that 
can be filled and plugged. No parsing of the actual string is done, but the 
string is broken up into `chunk`'s in between the `hole`'s.
-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE BangPatterns                 #-}
{-# LANGUAGE TypeAbstractions             #-}
{-# LANGUAGE TupleSections                #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.StringTemplate.TemplateInternal where

import Prelude                 hiding (null)
import GHC.TypeNats            (Natural)
import Data.Text               qualified as DT
import Data.List               (union
                               ,delete)
import Data.Maybe              (isNothing)

-- | A hole has an index and a possible filling.
type Hole = (Natural, Maybe DT.Text)

-- | Internal templates are the underlying structure of `Template`.
data ITemplate  where
    IChunk   :: DT.Text -> ITemplate
    ICompose :: DT.Text -> Hole -> ITemplate -> ITemplate

instance Show ITemplate where
    show :: ITemplate -> String    
    show (IChunk t)                           = DT.unpack t
    show (ICompose   prefix (i,Nothing) rest) = DT.unpack prefix <> "$" <> show i <> "{}" <> show rest
    show (ICompose   prefix (i,Just c) rest)  = DT.unpack prefix <> "$" <> show i <> "{"  <> DT.unpack c <>"}" <> show rest

-- | A template with pluggable holes. We do not expose the underlying
-- constructor in favor of the combinators.
data Template where
    Template :: ITemplate                             -- ^ Internal template
             -> ([Natural],Natural,[Natural],Natural) -- ^ Unfilled hole indices, number of unfilled holes, filled hole indices, and number of filled holes
             -> Template

-- | Pattern synonym for the empty template.
pattern Empty :: Template
pattern Empty <- (null -> True) where
    Empty = empty

-- | Pattern synonym for template chunk's.
pattern Chunk :: DT.Text -> Template
pattern Chunk s <- (Template (IChunk s) ([],0,[],0))
    where
        Chunk = chunk

-- | Pattern synonym for the composition of templates.
pattern Compose :: DT.Text -> Hole -> Template -> Template
pattern Compose c h t <- (decompose -> Just (c, h, t))
    where
        Compose = (compose)

{-# COMPLETE Chunk, Compose #-}

-- | Explicitly create a top-level composition template.
compose :: DT.Text -- ^ Prefix chunk
        -> Hole    -- ^ Hole index and potential filling
        -> Template -- ^ Template branch
        -> Template
compose c (i,Nothing) t = chunk c +> hole i     +> t
compose c (i,Just f)  t = chunk c +> filled i f +> t

-- | Decompose a template into the top-level compose.
decompose :: Template -> Maybe (DT.Text, Hole, Template)
decompose (Template (ICompose c h@(i,Nothing) t') (uh,nuh,fh,nfh)) = Just (c, h, (Template t' (i `delete` uh,nuh-1,fh,nfh)))
decompose (Template (ICompose c h@(i,Just _)  t') (uh,nuh,fh,nfh)) = Just (c, h, (Template t' (uh,nuh,i  `delete` fh,nfh-1)))
decompose _ = Nothing

-- | Test to see if a template is empty.
null :: Template -> Bool
null (Template (IChunk "") ([],0,[],0)) = True
null _ = False

newtype FilledTemplate = FilledTemplate Template

instance Show Template where
    show :: Template -> String
    show (Template t _) = show t

instance Show FilledTemplate where
    show :: FilledTemplate -> String
    show (FilledTemplate t) = show t

-- | Equality of `ITemplates`. The contents of filled holes are included in the
-- decision.
(>==>) :: ITemplate 
       -> ITemplate 
       -> Bool
(IChunk chk1)               >==> (IChunk chk2)               = chk1 == chk2
(ICompose   chk1 (_,c1) r1) >==> (ICompose   chk2 (_,c2) r2) = chk1 == chk2 && c1 == c2 && r1 >==> r2
_                          >==> _                          = False

instance Eq Template where
    (==) :: Template -> Template -> Bool
    (==) = (==>)

-- | Equality of templates. Two templates are considered equivalent if and only
-- if they differ by hole labels only. The contents of filled holes are included
-- in the decision.
(==>) :: Template
      -> Template
      -> Bool
(Template t1 _) ==> (Template t2 _) = t1 >==> t2    

-- | An empty hole.
hole :: Natural -- ^ Hole index
     -> Template
hole i = flip Template ([i],1,[],0) $ ICompose "" (i,Nothing) (IChunk "")

-- | A filled hole.
filled :: Natural -- ^ Hole index
       -> DT.Text -- ^ Hole Filling
       -> Template
filled i c = flip Template ([],0,[i],1) $ (ICompose "" (i, Just c) (IChunk ""))

-- | A chunk is a substring to a larger string.
chunk :: DT.Text -- ^ Substring.
      -> Template
chunk = flip Template ([],0,[],0) .  IChunk

-- | The empty template corresponds to the empty string.
empty :: Template
empty = chunk ""

-- | Composition of `ITemplates`.
(>+>) :: ITemplate
      -> ITemplate
      -> ITemplate
(IChunk chk1)    >+> (IChunk chk2)    = IChunk $ chk1 <> chk2
(IChunk chk)     >+> (ICompose p h r) = ICompose (chk <> p) h r
(ICompose p h r) >+> t               = (ICompose p h $ r >+> t) 

-- | Composition of templates.
(+>) :: Template
     -> Template
     -> Template
(Template t1 (ufhs1,m1,fhs1,n1)) +> (Template t2 (ufhs2,m2,fhs2,n2)) 
    = Template (t1 >+> t2) (ufhs1 `union` ufhs2,m1 + m2,fhs1 `union` fhs2,n1 + n2) 

-- | Convert a templates AST into a `Text`. The `Show` instance for `Template`
-- is set to pretty print, but for debugging it is sometimes useful to see the
-- raw AST.
showAST :: Template -> DT.Text
showAST (Template (IChunk x) _)                    = "IChunk "   <> (DT.show x)
showAST (Template (ICompose p (h, Nothing) r) hls) = "ICompose " <> (DT.show p) <> " " <> (DT.show h) <> " (" <> (showAST (Template r hls)) <> ")"
showAST (Template (ICompose p (h, Just c)  r) hls) = "ICompose " <> (DT.show p) <> " " <> (DT.show h) <> " " <> (DT.show c) <> " (" <> (showAST (Template r hls)) <> ")"

-- | Get the list of unfilled-hole indices present in a template.
-- Time complexity: @O(0)@
unfilledHoles :: Template  -- ^ Template 
              -> [Natural]
unfilledHoles (Template _ (hls,_,_,_)) = hls

-- | Get the list of filled-hole indices present in a template.
-- Time complexity: @O(0)@
filledHoles :: Template  -- ^ Template 
            -> [Natural]
filledHoles (Template _ (_,_,fhls,_)) = fhls

-- | Get the number of unfilled holes in a template.
-- Time complexity: @O(0)@
numberOfUnfilledHoles :: Template  -- ^ Template 
                      -> Natural
numberOfUnfilledHoles (Template _ (_,m,_,_)) = m

-- | Get the number of filled holes in a template.
-- Time complexity: @O(0)@
numberOfFilledHoles :: Template  -- ^ Template 
                    -> Natural
numberOfFilledHoles (Template _ (_,_,_,n)) = n

-- | Decide if a template is filled or not. 
-- Time complexity: \(\mathcal{O}(1)\)
isFilled :: Template -> Bool
isFilled t = numberOfUnfilledHoles t == 0

-- | Convert a template with no holes, a chunk, into a text.
-- Time complexity: @O(0)@
chunkToText :: Template      
            -> Maybe DT.Text
chunkToText (Template (IChunk c) ([],0,[],0)) = Just c
chunkToText _                                = Nothing

-- | Fill a hole with a text. Returns @Nothing@ if the hole index doesn't exist.
-- Filling a hole doesn't replace the hole, but simply puts the input text
-- inside the hole. 
fillHoleI :: ITemplate 
          -> Natural   -- ^ Hole index to plug
          -> DT.Text   -- ^ Hole filling
          -> Maybe ITemplate
fillHoleI (ICompose p (h,_) t) i c | h == i = do
    t' <- fillHoleI t i c
    Just $ ICompose p (h, Just c) t'
fillHoleI (ICompose p hl t) i c = do
    t' <- fillHoleI t i c
    Just $ ICompose p hl t'
fillHoleI _ _ _ = Nothing

-- | Fill a hole with a text. Returns @Nothing@ if the hole index doesn't exist.
-- Filling a hole doesn't replace the hole, but simply puts the input text
-- inside the hole.
fillHole :: Template
         -> Natural  -- ^ Hole index to plug
         -> DT.Text  -- ^ Hole filling
         -> Maybe Template
fillHole (Template t@(ICompose _ _ _) st@(hls,m,fhls,n)) i c | i `elem` hls || i `elem` fhls = do 
    t' <- fillHoleI t i c
    if i `elem` hls
    then Just $ Template t' (i `delete` hls,m - 1,i:fhls,n + 1)
    else if i `elem` fhls
         then Just $ Template t' st
         else Nothing
fillHole _ _ _ = Nothing

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHoleI :: ITemplate 
          -> Natural   -- ^ Hole index to plug
          -> DT.Text   -- ^ Text to replace hole
          -> Maybe ITemplate
plugHoleI (ICompose p (h,f) (IChunk s)) i c 
    | i == h && isNothing f = Just $ IChunk $ p <> c <> s
plugHoleI (ICompose p hl@(h,f) r@(ICompose p' h' s))  i c 
    | i == h && isNothing f = Just $ ICompose (p <> c <> p') h' s
    | otherwise = do r' <- plugHoleI r i c
                     Just $ ICompose p hl r'
plugHoleI _ _ _ = Nothing       

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHole :: Template 
         -> Natural  -- ^ Hole index to plug
         -> DT.Text  -- ^ Text to replace hole
         -> Maybe Template
plugHole (Template t@(ICompose _ _ _) (hls,m,fhls,n)) i c | i `elem` hls = 
        do t' <- plugHoleI t i c
           pure $ Template t' (i `delete` hls,m - 1,fhls,n)
plugHole _ _ _ = Nothing

-- | Plugs every hole in a template using the given plug function. If the plug
-- function is defined for every unfilled hole in the input template, then this function
-- guarantees a template with no holes (a text) is returned where all filled
-- holes are replaced with their filling.
plugAllI 
    :: (Natural -> Maybe DT.Text) -- ^ Plug function.
    -> ITemplate                  -- ^ ITemplate to plug.
    -> Maybe ITemplate
plugAllI f (ICompose chk (h,Nothing) r) = do
    chk' <- f h
    IChunk chk'' <- plugAllI f r
    return . IChunk $ chk <> chk' <> chk''
plugAllI f (ICompose chk (_, (Just c)) r) = do
    IChunk chk'' <- plugAllI f r
    return . IChunk $ chk <> c <> chk''
plugAllI _ t@(IChunk _) = return t

-- | Plugs every hole in a template using the given plug function. If the plug
-- function is defined for every unfilled hole in the input template, then this function
-- guarantees a template with no holes (a text) is returned where all filled
-- holes are replaced with their filling.
plugAll :: Template                                   -- ^ Template to plug
        -> ([Natural] -> (Natural -> Maybe DT.Text))  -- ^ Plug function
        -> Maybe DT.Text
plugAll (Template t (hls,_,_,_)) f = 
    case plugAllI (f hls) t of        
        Just (IChunk c) -> Just c
        _              -> Nothing

-- | Converts a filled template into a text. Returns @Nothing@ if the template
-- isn't filled.
filledToTextI :: ITemplate -> Maybe DT.Text
filledToTextI (IChunk c) = Just c
filledToTextI (ICompose p (_,Just c) t) = do
    t' <- filledToTextI t
    Just $ p <> c <> t'
filledToTextI _ = Nothing

-- | Converts a filled template into a text. Returns @Nothing@ if the template
-- isn't filled.
filledToText :: Template -> Maybe DT.Text
filledToText (Template t ([],0,_,_)) = do
    t' <- filledToTextI t
    Just t'
filledToText _ = Nothing

-- | The result of matching a filled template against a text.
data MatchResult = Matched          -- ^ Match successful
                 | Unmatched        -- ^ Match unsuccessful
                 | UnfilledTemplate -- ^ Template is unfilled
    deriving (Eq,Show)

-- | Match a filled template against some text. 
match :: Template -> DT.Text -> MatchResult
match (Template t ([],0,_,_)) s = 
    maybe UnfilledTemplate (\tt -> if s == tt then Matched else Unmatched) $ m
    where
        m = filledToTextI t
match _  _ = UnfilledTemplate
