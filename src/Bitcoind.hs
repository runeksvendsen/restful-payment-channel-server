module Bitcoind where

import Control.Monad.IO.Class (liftIO, MonadIO)
-- import Control.Monad.Reader (MonadReader)
-- import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
import Control.Exception (try)
import Network.Haskoin.Transaction as HT
import Network.Haskoin.Crypto as HC
import qualified Data.Bitcoin.Transaction as Btc
import Network.HTTP.Client (HttpException (..))
-- import Network.Bitcoin.RawTransaction
import Network.Bitcoin.Api.Client (Client, withClient)
import Network.Bitcoin.Api.Transaction (send)
import Data.HexString as Hex
import Data.Text as T
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL


data BTCRPCInfo = BTCRPCInfo {
    rpcIP    :: String
    ,rpcPort :: Int
    ,rpcUser :: T.Text
    ,rpcPass :: T.Text
}

bitcoindNetworkSumbitTx :: BTCRPCInfo -> HT.Tx -> IO (Either String HT.TxHash)
bitcoindNetworkSumbitTx (BTCRPCInfo ip port user pass) tx =
    withClient ip port user pass
        (tryBitcoindSubmitToNetwork tx)

tryBitcoindSubmitToNetwork ::
    MonadIO m =>
    HT.Tx ->
    Client
    -> m (Either String HT.TxHash)
tryBitcoindSubmitToNetwork tx conn = do
    res <- liftIO $ try $ send conn $ Btc.decode . fromBytes $ serialize tx
    case res of
        Left e -> return $
            Left $ "Error submitting Bitcoin transaction to network. reason: "
                ++ show (e :: HttpException)
        Right txid -> do
            liftIO . putStrLn $ "Submitted Bitcoin transaction to the network: " ++
                T.unpack (Hex.toText txid)
            return $ maybe
                (Left $ "BUG: failed to parse transaction ID returned by bitcoind: " ++ show txid)
                Right
                (HT.hexToTxHash (B16.encode $ toBytes txid))

serialize :: Bin.Binary a => a -> BS.ByteString
serialize = BL.toStrict . Bin.encode

-- StatusCodeException
--  (Status {statusCode = 500,
--  statusMessage = \"Internal Server Error\"})
--  [(\"Content-Type\",\"application/json\"),(\"Date\",\"Sat, 30 Apr 2016 18:15:25 GMT\"),(\"Content-Length\",\"83\"),(\"X-Response-Body-Start\",\"{\\\"result\\\":null,\\\"error\\\":{\\\"code\\\":-26,\\\"message\\\":\\\"64: too-long-mempool-chain\\\"},\\\"id\\\":1}\\n\"),(\"X-Request-URL\",\"POST http://192.168.1.102:8334/\")] (CJ {expose = []})
