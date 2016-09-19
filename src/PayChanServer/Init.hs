{-# LANGUAGE OverloadedStrings #-}

module  PayChanServer.Init where

import           Common.Util
import           PayChanServer.Util (dummyKey)
import           PayChanServer.Config.Types
import           PayChanServer.Config.Util
import qualified PayChanServer.Settlement as Settle
import qualified Test.Dummy as Dummy

import           PayChanServer.DB (initWaitConnect)
import           ChanStore.Interface  as DBConn
import           SigningService.Interface (getPubKey)
import qualified BlockchainAPI.Impl.Bitcoind.Interface as Btc
import qualified BlockchainAPI.Types as BtcAPI

import           Control.Monad                  (void)
import qualified System.Posix.Signals as Sig
import           Control.Concurrent             (ThreadId, throwTo)
import qualified Control.Exception as E
import           Data.Maybe                     (isNothing)
import           System.IO                      (hFlush, stdout)


appConfInit :: Config -> IO App
appConfInit cfg = do
    -- Debug
    debug <- configDebugIsEnabled cfg
    when debug $ do
        putStrLn "############# RUNNING IN DEBUG MODE #############"
        putStrLn "## Fake funding accepted & settlement disabled ##"
        putStrLn "#################################################"

    bitcoinNetwork <- configLookupOrFail cfg "bitcoin.network"
    setBitcoinNetwork bitcoinNetwork

    (ServerSettleConfig settleFee _) <- getServerSettleConfig cfg

    chanConfig <- getChanConf cfg

    signingServiceConn <- getSigningServiceConn cfg
    btcIface <- getBlockchainIface cfg

    dbIface <- getChanStoreIface =<< getDBConf cfg
--     putStr "Testing database connection... "
--     maybeRes <- initWaitConnect "database" $ DBConn.chanGet dbIface dummyKey
--     putStrLn $ if isNothing maybeRes then "success." else "success... but dummy key is present in database."

    putStr "Contacting SigningService for public key... "
    pubKey <- initWaitConnect "SigningService" $ getPubKey signingServiceConn
    putStrLn $ "success: " ++ cs (pathParamEncode pubKey)

    unless debug $ do
        putStr "Testing Blockchain service... " >> hFlush stdout
        either fail (\_ -> putStrLn "success.") =<<
            (Dummy.getAddr >>= Btc.unspentOuts btcIface)

    let basePathVersion = "/v1"
    settlePeriod <- configLookupOrFail cfg "settlement.settlementPeriodHours"

    let settleFunc = Settle.finishSettleChannel dbIface signingServiceConn btcIface settleFee

    return $ App dbIface
                 (BtcAPI.listUnspentOutputs btcIface) settleFunc
                 pubKey
                 chanConfig settlePeriod
                 basePathVersion
                 debug

installHandlerKillThreadOnSig :: Sig.Signal -> ThreadId -> IO ()
installHandlerKillThreadOnSig sig tid =
    void (Sig.installHandler
          sig
          (Sig.CatchInfo $ \ci -> do
              putStrLn ("Received signal: " ++
                  show (Sig.siginfoSignal ci) ++
                  ". Killing main thread...")
              throwTo tid E.UserInterrupt)
          Nothing)

