{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NumericUnderscores #-}

module PlutusTestv10clean where

import           Control.Monad              hiding (fmap)
import qualified Control.Monad.Freer.Extras as Extras
import qualified Data.Aeson                 as Aeson
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text, pack)
import           Ledger.Ada                 as Ada
import           Ledger                     hiding (mint, singleton)
import           Ledger.TimeSlot
import           Ledger.Value               as Value
import           Plutus.Contract            as Contract
import qualified Plutus.Trace               as Trace
import           Plutus.Trace.Emulator      as Emulator
import           Plutus.Trace.Emulator.Types
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..), IO, show)
import qualified Prelude                    as P
import           Wallet.Emulator.Wallet
import           Wallet.Emulator.MultiAgent
import           System.IO

-- 20220623 import the working modules
import           Apps.CtMintShop         (TokenParams (..), ListParams(..), BurnParams (..),
                                          MintSendParams (..), TokenPolicyParams (..))
import           Apps.CtMarketPlace      (StartParams (..), BidParams (..), CloseParams (..))
import           Apps.CtOracleCore
import           Apps.CtOracleSwap
import           Apps.CtSchema

customShowEvent :: EmulatorEvent' -> Maybe P.String
customShowEvent = \case
  UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg <> "\n"
  InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _)   -> Just $ "*** CONTRACT LOG: " <> P.show msg <> "\n"
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> P.show err <> "\n"
  ev                                                                   -> Nothing

traceConfig :: Trace.TraceConfig
traceConfig =
  Trace.TraceConfig
    customShowEvent
    stdout

emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig (Left $ Map.fromList [(knownWallet 1, v1),  (knownWallet 2, v2), 
                                (knownWallet 3, v3), (knownWallet 4, v4) ]) def def
  where

    -- current settings- every wallets holds 100 ADA and 10 USDT Tokens so 
    -- that changes (additions and deductions) can be more easily observed 
    -- user may varies the wallets holdings accordingly

    v1 :: Value
    v1 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 10_000
    v2 :: Value
    v2 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 10_000
    v3 :: Value
    v3 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 10_000
    v4 :: Value
    v4 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 10_000

-- USDT is defined as below:

assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = tokenName "USDT"

token :: AssetClass
token = AssetClass (assetSymbol, assetToken)

{-- 20220629 - try again later 
checkOracle :: Oracle -> Contract () w Text ()
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle
--}

-- preparing for the test of the workflow

testWorkFlow :: IO ()
testWorkFlow = Trace.runEmulatorTraceIO' traceConfig emCfg myTrace

myTrace :: Trace.EmulatorTrace ()
myTrace = do

-- # SCENE 1 # - Pre-Allocation of 100_000_000 USDT and 10_000 lovelace to the respective knownwallets 1-4 

    let wallets = P.show $ P.map P.show knownWallets
    Extras.logInfo $ "Wallets: " ++ wallets    

-- # SCENE 2 # - Mint the Oracle NFT for Wallet 1 

    let tn = "OracleNFT"

    h1_1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints1 
    h2_1 <- Trace.activateContractWallet (knownWallet 2) $ endpoints1
    h3_1 <- Trace.activateContractWallet (knownWallet 3) $ endpoints1
    h4_1 <- Trace.activateContractWallet (knownWallet 4) $ endpoints1

    Trace.callEndpoint @"mint" h1_1 $ TokenParams
        { tpToken  = tn
        , tpAmount = 1                                  -- is always 1 for NFT
        , tpAddress = mockWalletAddress (knownWallet 1)
        }

    s <- Trace.waitNSlots 1

    h1_2 <- Trace.activateContractWallet (knownWallet 1) $ endpoints2 
    h2_2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints2
    h3_2 <- Trace.activateContractWallet (knownWallet 3) $ endpoints2
    h4_2 <- Trace.activateContractWallet (knownWallet 4) $ endpoints2

    -- let's see the values of the oracle NFT!

    Trace.callEndpoint @"list" h1_2 $ ListParams
        { lpAddress = mockWalletAddress (knownWallet 1)
        , lpWalletName = P.show (knownWallet 1)
        }

    void $ Trace.waitNSlots 2

    -- checkpoint01: 
    -- Slot 00001: *** CONTRACT LOG: "-------------------- Start Minting --------------------"
    -- Slot 00001: *** CONTRACT LOG: "picked UTxO at TxOutRef {txOutRefId = 5f920566627190ccf354f3c5da52d601f5fc44bfe371f940c0dc57c1c9ccca81, txOutRefIdx = 3}"
    -- Slot 00002: *** CONTRACT LOG: "-------------------- Listing for Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491 --------------------"
    -- Slot 00002: *** CONTRACT LOG: "Current token(s):  [[(,\"\",2000000),(06d31dbc1f726f6eda3d54e5b98134cadba0bc5775d5a5596cfe0f9e,\"OracleNFT\",1)]]"

-- # SCENE 3 # - Start the Oracle by Wallet 1!!!!

    let cs = "06d31dbc1f726f6eda3d54e5b98134cadba0bc5775d5a5596cfe0f9e"
        op = OracleParams
                { opOracleSymbol = cs
                , opOracleTokenName = tn
                , opFee = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                , opExchange = 2                
                }

    h1_3 <- Trace.activateContractWallet (knownWallet 1) $ runOracle op
    void $ Trace.waitNSlots 1
    oracle <- getOracle h1_3

    -- h2 <- activateContractWallet (knownWallet 2) $ checkOracle oracle
    void $ Trace.waitNSlots 1

-- # SCENE 4 # - Oracle (Wallet 1) initialise its first exchange rate

    Trace.callEndpoint @"update" h1_3 2
    
    void $ Trace.waitNSlots 3

-- # SCENE 5 # - Wallet 4 wants some ADA in exchange for USDT - Let's do some swap!!!

    h3_3 <- Trace.activateContractWallet (knownWallet 3) $ swap oracle
    h4_3 <- Trace.activateContractWallet (knownWallet 4) $ swap oracle

    -- Wallet 3 offer to sell 10 ADA based on prevailing exchange rate

    Trace.callEndpoint @"offer" h3_3 10_000_000
    
    void $ Trace.waitNSlots 1

    -- Wallet 4 to buy ADA based on prevailing exchange rate

    Trace.callEndpoint @"use" h4_3 20
    
    void $ Trace.waitNSlots 3

-- # SCENE 6 # - Wallet 1 to update its Oracle to raise its exchange rate

    Trace.callEndpoint @"update" h1_3 3
    void $ Trace.waitNSlots 3

-- # SCENE 7 # - Wallet 4 now wants to create NFt and sent for Auction immediately

    let tn4 = "W4_NFT"

    Trace.callEndpoint @"mintsend" h4_2 $ MintSendParams
        { msToken    = tn4
        , msDeadline = slotToBeginPOSIXTime def 10
        , msMinBid   = 15_000_000
        , msAddress  = mockWalletAddress (knownWallet 4)
        } 

    void $ Trace.waitNSlots 3

    -- checkpoint02:
    -- Slot 00016: *** CONTRACT LOG: "-------------------- Start Minting --------------------"
    -- Slot 00016: *** CONTRACT LOG: "picked UTxO at TxOutRef {txOutRefId = b80d3d64bb125c199c303b88b3bbc8e84e9f9bf7078b005fc42679839e08657c, txOutRefIdx = 0}"
    -- Slot 00017: *** CONTRACT LOG: "The currency symbol is fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d"
    -- Slot 00017: *** CONTRACT LOG: "-------------------- Listing for Seller --------------------"
    -- Slot 00017: *** CONTRACT LOG: "Current token(s):  [[(,\"\",2000000),(fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d,\"W4_NFT\",1)]]"

-- # SCENE 8 # - Let the bidding begins between Wallets 1,2,3

    let cs4 = "fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d"  -- currency symbol of token "W4_NFT"
    
    void $ Trace.waitUntilSlot 2

    Trace.callEndpoint @"bid" h3_2 (BidParams cs4 tn4 20_000_000)
    s <- Trace.waitNSlots 2
    
    Trace.callEndpoint @"bid" h1_2 (BidParams cs4 tn4 10_000_000)
    s <- Trace.waitNSlots 2
    
    Trace.callEndpoint @"bid" h2_2 (BidParams cs4 tn4 60_000_000)
    s <- Trace.waitNSlots 10

    -- Time's up (10 slots) and winner merge

    Trace.callEndpoint @"close" h4_2 (CloseParams cs4 tn4)
    s <- Trace.waitNSlots 1
    
-- # SCENE 9 # - Wallet 2 wants to mint Tokens and decide to create 8 identical ones 

    let tn2  = "W2_Tokens"
        val2 = 8

    -- h2_1 <- Trace.activateContractWallet (knownWallet 2) $ endpoints1

    Trace.callEndpoint @"mint" h2_1 $ TokenParams
        { tpToken  = tn2
        , tpAmount = val2                                  -- minting more than 1 tokens
        , tpAddress = mockWalletAddress (knownWallet 2)
        }

    s <- Trace.waitNSlots 3

    -- let's see the values of the oracle NFT!
    
    h2_2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints2
    
    -- let's see the values of the oracle NFT!

    Trace.callEndpoint @"list" h2_2 $ ListParams
        { lpAddress = mockWalletAddress (knownWallet 2)
        , lpWalletName = P.show (knownWallet 2)
        }

    s <- Trace.waitNSlots 1

    -- checkpoint05:
    -- Slot 00035: *** CONTRACT LOG: "-------------------- Start Minting --------------------"
    -- Slot 00035: *** CONTRACT LOG: "picked UTxO at TxOutRef {txOutRefId = 5f920566627190ccf354f3c5da52d601f5fc44bfe371f940c0dc57c1c9ccca81, txOutRefIdx = 2}"
    -- Slot 00038: *** CONTRACT LOG: "-------------------- Listing for Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58 --------------------"
    -- Slot 00038: *** CONTRACT LOG: "Current token(s):  [[(,\"\",2000000),(b6ce6e35ecd1d1bf7c7641bc46f6de3692c9e122897eab9a7dbed4e3,\"W2_Tokens\",8)]]"


-- # SCENE 10 # - Wallet 2 somehow thinks that having 8 identical tokens are too many, decides to burn some (6)
-- Important note* - only person who owns the tokens and has its token policy can then burn his tokens.
-- The token policy is part of the 3 parts formula function of the TxOutRef of the original creation
-- the other 2 being the tokenname and its original value. 
-- Note: The original TxOutRef: {txOutRefId = 5f920566627190ccf354f3c5da52d601f5fc44bfe371f940c0dc57c1c9ccca81, txOutRefIdx = 2}
    
    let tkp = TokenPolicyParams 
            { tkpOref   = TxOutRef 
                {
                  txOutRefId = "5f920566627190ccf354f3c5da52d601f5fc44bfe371f940c0dc57c1c9ccca81"
                , txOutRefIdx = 2  
                }
                , tkpToken  = tn2
                , tkpAmount = val2
            }
    
        cs2 = "b6ce6e35ecd1d1bf7c7641bc46f6de3692c9e122897eab9a7dbed4e3"  
    
    Trace.callEndpoint @"burn" h2_2 $ BurnParams
        { btCurrency = cs2 
        , btToken  = tn2
        , btAmount = -6
        , btAddress = mockWalletAddress (knownWallet 2)
        , btPolicy = tkp
        }

    void $ Trace.waitNSlots 1  

-- # SCENE 11 # - Wallet 2 decides to finally to reduce the token value to 1

    Trace.callEndpoint @"burn" h2_2 $ BurnParams
        { btCurrency = cs2 
        , btToken  = tn2
        , btAmount = -1
        , btAddress = mockWalletAddress (knownWallet 2)
        , btPolicy = tkp
        }

    void $ Trace.waitNSlots 3

-- # SCENE 12 # - Wallet 2 finally decides to send his only token to the auction
    
    Trace.callEndpoint @"start" h2_2 $ StartParams
        { spDeadline     = slotToBeginPOSIXTime def 10
        , spMinBid       = 5_000_000
        , spCurrency     = cs2
        , spToken        = tn2
        }
    
    void $ Trace.waitUntilSlot 3

-- # SCENE 13 # - Wallet 3 wants to try his luck at auction again... and the bidding ensues...

    h3_2 <- Trace.activateContractWallet (knownWallet 3) $ endpoints2

    Trace.callEndpoint @"bid" h3_2 (BidParams cs2 tn2 20_000_000)
    
    void $ Trace.waitUntilSlot 3

-- END --}

  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Trace.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
    