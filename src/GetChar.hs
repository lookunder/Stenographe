{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Module      :  GetChar
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  charles.stpierre@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module GetChar (
  GetChar.getChar
) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
foreign import ccall unsafe "conio.h getch" getChar :: IO Char
#else
import qualified System.IO as SIO

getChar :: IO Char
getChar = SIO.getChar
#endif



