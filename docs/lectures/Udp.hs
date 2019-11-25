module UDP where 
  import Prelude hiding (length, splitAt)
  
  -------------------------------------------------------------------------------
  -- | Speedup Parsing 
  -------------------------------------------------------------------------------
  
  data UDP = UDP {
      udpSrcPort  :: Text -- 2 characters  
    , udpDestPort :: Text -- 2 characters  
    , udpLength   :: Text -- 2 characters
    , udpCheckSum :: Text -- 2 characters
  }
 
  
  updParser :: Text -> UDP 
  updParser t = 
      let (udp1, t1) = splitAt 2 t 
          (udp2, t2) = splitAt 2 t1 
          (udp3, t3) = splitAt 2 t2 
          (udp4, _ ) = splitAt 2 t3
      in UDP udp1 udp2 udp3 udp4
  
  splitAt :: Int -> Text -> (Text, Text)
  splitAt i t
    | i <= tlen t 
    = unsafeSplitAt i t 
    | otherwise 
    = error "unsafe splitAt"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  -------------------------------------------------------------------------------
  -- Appendix 
  -------------------------------------------------------------------------------
  data Text = Text String 
  
  {-@ die :: {v:String | false} -> a @-}
  die :: String -> a 
  die = error 
  
  {-@ unsafeSplitAt 
       :: i:Int 
       -> t:Text -> ({tl:Text | tlen tl == i }, {tr:Text | tlen tr == tlen t - i }) @-}
  unsafeSplitAt :: Int -> Text -> (Text, Text)
  unsafeSplitAt _ _ = undefined 
  
  {-@ measure tlen @-}
  tlen :: Text -> Int
  tlen (Text ts) = length ts 
  
  {-@ measure length @-}
  length :: [a] -> Int 
  length []     = 0 
  length (_:xs) = 1 + length xs 
  