{-# LANGUAGE OverloadedStrings #-}

module  PayChanServer.Init where

import qualified AppPrelude.Types.Config as Conf
import           Common.Util
import           PayChanServer.Config.Types
import           PayChanServer.Config.Util
import qualified PayChanServer.Settlement as Settle
import qualified Test.Dummy as Dummy

import           PayChanServer.DB (initWaitConnect)
import           SigningService.Interface (getPubKey)
import qualified BlockchainAPI.Impl.Bitcoind.Interface as Btc
import qualified BlockchainAPI.Types as BtcAPI

import           Control.Monad                  (void)
import qualified System.Posix.Signals as Sig
import           Control.Concurrent             (ThreadId, throwTo)
import qualified Control.Exception as E
import           System.IO                      (hFlush, stdout)


appConfInit :: DB.DBConf db => Config -> IO (App db)
appConfInit cfg = do
    -- Debug
    debug <- configDebugIsEnabled cfg
    when debug $ do
        putStrLn "############# RUNNING IN DEBUG MODE #############"
        putStrLn "## Fake funding accepted & settlement disabled ##"
        putStrLn "#################################################"

    bitcoinNetwork <- configLookupOrFail cfg "bitcoin.network"
    setBitcoinNetwork bitcoinNetwork

    chanConfig <- getChanConf cfg

    signingServiceConn <- getSigningServiceConn cfg
    btcIface <- getBlockchainIface cfg

    dbConf  <- DB.init logLvl
    callbackIface <- Conf.fromConf cfg
    -- maybePort <- maybe Nothing readMaybe <$> lookupEnv "PORT"
    putStr "Contacting SigningService for public key... "
    pubKey <- initWaitConnect "SigningService" $ getPubKey signingServiceConn
    putStrLn $ "success: " ++ cs (pathParamEncode pubKey)

    unless debug $ do
        putStr "Testing Blockchain service... " >> hFlush stdout
        either fail (\_ -> putStrLn "success.") =<<
            (Dummy.getAddr >>= Btc.unspentOuts btcIface)

    let basePathVersion = "/v1"
    settlePeriod <- configLookupOrFail cfg "settlement.settlementPeriodHours"

    (ServerSettleConfig settleFee _) <- getServerSettleConfig cfg
    let settleFunc = Settle.finishSettleChannel dbIface signingServiceConn btcIface settleFee

    return $ App dbConf
                 callbackIface
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

