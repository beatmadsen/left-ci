module Server.DataStore
  ( BuildStore (..),
  )
where

data BuildStore = BuildStore { demo :: Int -> String }
