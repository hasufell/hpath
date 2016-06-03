-- |
-- Module      :  HPath.IO.Utils
-- Copyright   :  © 2016 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Random and general IO/monad utilities.


module HPath.IO.Utils where


import Control.Monad
  (
    when
  , unless
  )


-- |If the value of the first argument is True, then execute the action
-- provided in the second argument, otherwise do nothing.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb a = mb >>= (`when` a)


-- |If the value of the first argument is False, then execute the action
-- provided in the second argument, otherwise do nothing.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb a = mb >>= (`unless` a)
