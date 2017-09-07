{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO Can we remove UndecidableInstances somehow?
module Lib where

import           Data.Monoid  ((<>))
import           Data.Proxy   (Proxy (Proxy))
import           Data.Text    (Text)
import qualified Data.Text    as Text
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

------------------------------------------------------------
-- Universe
data fmt1 :> fmt2

------------------------------------------------------------
-- Interpret the type as a formatting function.
class HasFormatter a where
  type Formatter a r :: *
  formatter :: Proxy a -> (Text -> r) -> Formatter a r

instance HasFormatter Text where
  type Formatter Text r = (Text -> r)
  formatter _ ret = ret

instance HasFormatter Int where
  type Formatter Int r = (Int -> r)
  formatter _ ret = ret . Text.pack . show

instance (KnownSymbol a) =>
         HasFormatter (a :: Symbol) where
  type Formatter a r = r
  formatter _ ret = ret (Text.pack (symbolVal (Proxy :: Proxy a)))

instance (HasFormatter fmt1, HasFormatter fmt2) =>
         HasFormatter (fmt1 :> fmt2) where
  type Formatter (fmt1 :> fmt2) r = Formatter fmt1 (Formatter fmt2 r)
  formatter _ ret =
    formatter
      (Proxy :: Proxy fmt1)
      (\v1 -> formatter (Proxy :: Proxy fmt2) (\v2 -> ret (v1 <> v2)))

toFmt
  :: HasFormatter a
  => Proxy a -> Formatter a Text
toFmt proxy = formatter proxy id

------------------------------------------------------------
-- Interpret the type as a C-style printf template.
class HasPrintf a where
  printf :: Proxy a -> Text

instance HasPrintf Text where
  printf _ = "%s"

instance HasPrintf Int where
  printf _ = "%i"

instance KnownSymbol a =>
         HasPrintf (a :: Symbol) where
  printf _ = Text.pack $ symbolVal (Proxy :: Proxy a)

instance (HasPrintf l, HasPrintf r) =>
         HasPrintf (l :> r) where
  printf _ = printf (Proxy :: Proxy l) <> printf (Proxy :: Proxy r)

------------------------------------------------------------
-- Example definition
demoType :: Proxy (Text :> " x" :> Int)
demoType = Proxy

-- Returns `("Apple x10", "%s x%i")
demo :: (Text, Text)
demo = (toFmt demoType "Apples" 10, printf demoType)
