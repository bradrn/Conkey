{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Convert.Intermediate
       ( Intermediate
       , convert
       ) where

import qualified Data.Map.Strict as Map

import qualified MSKLC.Keyboard as MS

-- | An intermediate type for keyboard layouts, represented as a list
-- of 2-tuples. The first element of each tuple is a key sequence
-- represented as a list of keys, where each key is given as a
-- (shift state, key code) tuple; the second element of each tuple is
-- the character resulting from that key sequence.
type Intermediate = [([(MS.ShiftState, MS.VkCode)], Char)]

-- | Convert an MSKLC keyboard to its 'Intermediate' representation.
convert :: MS.Keyboard -> Intermediate
convert k = toIntermediate (getReverseMappings k) k

-- | Given an 'MS.Keyboard', create a map from each character to the
-- key(s) used to type that character (excluding characters which are
-- input with two-key sequences using dead keys).
getReverseMappings :: MS.Keyboard -> [(Char, (MS.ShiftState, MS.VkCode))]
getReverseMappings (MS.Keyboard _ l) = do
    (vk, charAssign) <- Map.toList l
    (ss, entry) <- Map.toList charAssign
    case entry of
        MS.Unassigned -> []
        MS.Entry c -> return (c, (ss, vk))
        MS.DeadKey d -> return (MS.dkBaseChar d, (ss, vk))

-- | Given an MSKLC keyboard, as well as a set of reverse mappings as
-- generated by 'getReverseMappings', convert that keyboard to its
-- 'Intermediate' representation.
toIntermediate :: [(Char, (MS.ShiftState, MS.VkCode))] -> MS.Keyboard -> Intermediate
toIntermediate revMaps (MS.Keyboard _ l) = do
    (vk, charAssign) <- Map.toList l
    (ss, entry) <- Map.toList charAssign
    case entry of
        MS.Unassigned -> []
        MS.Entry c -> return ([(ss, vk)], c)
        MS.DeadKey MS.DeadKeyDesc{..} -> do
            (k2, v) <- Map.toList dkMapping
            key2 <- lookups k2 revMaps
            return ([(ss, vk), key2], v)
  where
    lookups :: Eq a => a -> [(a,b)] -> [b]
    lookups a = fmap snd . filter ((==a) . fst)
