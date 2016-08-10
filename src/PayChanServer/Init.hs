{-# LANGUAGE OverloadedStrings #-}

module  PayChanServer.Init where


import           PayChanServer.App  (mainRoutes)

import           PayChanServer.Util (dummyKey)
import           PayChanServer.Config.Types
import           PayChanServer.Config.Util
import           PayChanServer.Types (ServerSettleConfig(..))
import qualified PayChanServer.Settlement as Settle
import qualified Test.Dummy as Dummy

import           PayChanServer.DB (initWaitConnect)
import           ChanStore.Interface  as DBConn
import           SigningService.Interface (getPubKey)
import qualified BlockchainAPI.Impl.Bitcoind.Interface as Btc
import qualified BlockchainAPI.Types as BtcAPI

import           Common.URLParam (pathParamEncode)

import           Snap (SnapletInit, makeSnaplet, addRoutes)
import           Data.String.Conversions (cs)
import           Control.Monad          (when, unless, void)
import           Control.Monad.IO.Class (liftIO)
import qualified System.Posix.Signals as Sig
import           Control.Concurrent (ThreadId, throwTo)
import qualified Control.Exception as E
import           Data.Maybe (isNothing)
import           System.IO (hFlush, stdout)


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

    openConfig@(OpenConfig _ basePrice addSettleFee _) <- getChanOpenConf cfg
    let confOpenPrice = if addSettleFee then basePrice + settleFee else basePrice

    signingServiceConn <- getSigningServiceConn cfg
    btcIface <- getBlockchainIface cfg

    dbIface <- getChanStoreIface =<< getDBConf cfg
    putStr "Testing database connection... "
    maybeRes <- initWaitConnect "database" $ DBConn.chanGet dbIface dummyKey
    putStrLn $ if isNothing maybeRes then "success." else "success... but dummy key is present in database."

    putStr "Contacting SigningService for public key... "
    pubKey <- initWaitConnect "SigningService" $ getPubKey signingServiceConn
    putStrLn $ "success: " ++ cs (pathParamEncode pubKey)

    unless debug $ do
        putStr "Testing Blockchain service... " >> hFlush stdout
        either fail (\_ -> putStrLn "success.") =<<
            (Dummy.getAddr >>= Btc.unspentOuts btcIface)

    let basePathVersion = "/v1"
    settlePeriod <- configLookupOrFail cfg "settlement.settlementPeriodHours"

    -- We could also move this into 'getBlockchainIface'...
    let settleFunc = Settle.settleChannel dbIface signingServiceConn btcIface settleFee

    return $ App dbIface
                 (BtcAPI.listUnspentOutputs btcIface) settleFunc
                 pubKey
                 openConfig confOpenPrice settlePeriod
                 basePathVersion
                 debug

appInit :: Config -> SnapletInit App App
appInit cfg = makeSnaplet "PayChanServer" "RESTful Bitcoin payment channel server" Nothing $ do
    appConf@(App _ _ _ _ _ _ _ basePathVersion _) <- liftIO $ appConfInit cfg
    addRoutes $ mainRoutes basePathVersion
    return appConf

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

