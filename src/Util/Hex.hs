module Util.Hex where

import qualified Data.Serialize as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Bitcoin.PaymentChannel.Types as BTC

class Bin.Serialize a => HexBinEncode a where
    hexEncode :: a -> BS.ByteString
    hexEncode = B16.encode . Bin.encode

class Bin.Serialize a => HexBinDecode a where
    hexDecode :: BS.ByteString -> Either String a
    hexDecode = Bin.decode . fst . B16.decode

instance HexBinEncode BS.ByteString where hexEncode = B16.encode
instance HexBinEncode BTC.RecvPubKey
instance HexBinEncode BTC.SendPubKey
instance HexBinEncode HS.Script
instance HexBinEncode HC.Signature
instance HexBinEncode HT.TxHash

instance HexBinDecode BS.ByteString where hexDecode = Right . fst . B16.decode
instance HexBinDecode BTC.RecvPubKey
instance HexBinDecode BTC.SendPubKey
instance HexBinDecode HS.Script
instance HexBinDecode HC.Signature
instance HexBinDecode HT.TxHash