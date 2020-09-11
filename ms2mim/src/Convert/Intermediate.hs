{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Convert.Intermediate
       ( Intermediate
       , convert
       ) where

import qualified Data.Map.Strict as Map

import qualified MSKLC.Keyboard as MS

type Intermediate = [([(MS.ShiftState, MS.VkCode)], Char)]

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
