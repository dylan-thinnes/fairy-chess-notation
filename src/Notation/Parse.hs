module Notation.Parse where

import Notation hiding (move)

import Text.Parsec
import Control.Lens
import Data.List
import Data.Maybe (fromMaybe)

type Parser m a = ParsecT String () m a

-- Simple parser for integral values
integral :: (Monad m, Read a, Integral a) => ParsecT String () m a
integral = read <$> choice [(:) <$> char '-' <*> pos, pos]
    where
    pos = many1 digit

-- Main entry point for parsing moves from strings
parseMove :: String -> Either ParseError MoveTree
parseMove = runParser Notation.Parse.move () "<debug>"

-- Moves
move :: (Monad m) => Parser m MoveTree
move = sum
    where
    sum = do
        -- Parse in as many products as possible
        product <- product
        rest <- optionMaybe $ do
            string ","
            sum
        pure $ case rest of
                  Nothing   -> product
                  Just rest -> product :+: rest

    product = do
        -- Parse in as many modified moves as possible
        modified <- modified
        rest <- optionMaybe $ do
            string "."
            product
        pure $ case rest of
                  Nothing   -> modified
                  Just rest -> modified :*: rest

    modified = do                                      
        move <- choice                        -- Parse in moves followed by modifiers
           [ BaseMove <$> baseMove                  -- Either a basemove
           , between (string "(") (string ")") move -- Or a pair of parens with another move
           ]
        mods <- modifiers                     -- Parse in modifiers
        pure $ if null mods                   -- If there are no modifiers...
                  then move                         -- Just return move
                  else Modified move mods           -- Otherwise, return modified move

baseMove :: (Monad m) => Parser m Delta
baseMove = fmap Delta
         $ between (string "[") (string "]")
         $ sepBy1 integral (string ",")

-- Parse any modifier, or multiple modifiers to then compose
modifiers :: (Monad m) => Parser m [Modifier]
modifiers = many modifier
modifier :: (Monad m) => Parser m Modifier
modifier = choice [mirrorX, mirrorY, swapXY, mirrorXY, mirrorSwapXY, expGroups, ranges]

-- Modifiers for axis mirroring / swapping
mirrorX, mirrorY, swapXY, mirrorXY, mirrorSwapXY :: (Monad m) => Parser m Modifier
mirrorX      = char '|' >> return (Mirror 0)
mirrorY      = char '-' >> return (Mirror 1)
swapXY       = char '/' >> return (Swap 0 1)
mirrorXY     = char '+' >> return (MirrorXY)
mirrorSwapXY = char '*' >> return (MirrorXYSwap)

expGroups :: (Monad m) => Parser m Modifier
expGroups = fmap Exponents
         $ between (string "{") (string "}")
         $ sepBy1 expGroup (string ",")
    where

    -- Parse in either a range, or a single integer, as a list of integers
    expGroup = choice [startsWithDotDot, startsWithInteger]
    startsWithDotDot = Range Nothing <$> rangeEnd
    startsWithInteger = do
        start <- integral
        mayRange <- optionMaybe rangeEnd
        case mayRange of
          (Just mayEnd) -> pure $ Range (Just start) mayEnd
          Nothing       -> pure $ Single start

    -- Optional range
    rangeEnd = do
        string ".."
        optionMaybe integral

ranges :: (Monad m) => Parser m Modifier
ranges = do
    chars <- choice $ map (many1 . char) ['^','V','>','<']
    let len = fromIntegral $ length chars - 1
    pure $ case head chars of
              '^' -> Above 1 len
              'V' -> Below 1 len
              '>' -> Above 0 len
              '<' -> Below 0 len
