{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE KindSignatures #-}

module Types where


import CLaSH.Prelude
-- import CLaSH.Sized.BitVector


type WordSize = BitVector 32
type AddrSize = BitVector 15
