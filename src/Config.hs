module Config where

import Data.Configurator.Types
import qualified Network.Haskoin.Transaction as HT
import qualified Crypto.Secp256k1 as Secp

pubKeyServer = HC.derivePubKey prvKeyServer
prvKeyServer = HC.makePrvKey $ fromJust $ Secp.secKey $ fromHexString
                       "456af7395529404380edc4fa35a161e096aa510610b98d3d219dc178dc58c1d7"
