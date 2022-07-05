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

-- module CtMinShop - enables the minting and burning of NFT and Tokens 
-- depending on the 'amount' parameters entered)  
-- other mint related functions include:
-- "mintAndSend" - immediate creation of tokens and sent for auction
-- "listing" - off chian function to list existing tokens in the wallet

module Apps.CtMintShop
    ( TokenParams (..)
    , BurnParams (..)
    , TokenPolicyParams (..)
    , MintSendParams (..)
    , ListParams (..)
    , mintToken
    , burnToken
    , mintSendToken
    , listTokens
    , tokenPolicy
    , tokenCurSymbol
    --, MintSchema
    --, BurnSendListSchema
    --, endpoints1
    --, endpoints2
    ) where

import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Ledger                      hiding (mint, singleton)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified Prelude                     as P
import           Prelude                     (Semigroup (..), Show (..), String)
import           Text.Printf                 (printf)

import           Apps.Utils                  (getCredentials)
import           Apps.CtMarketPlace          (StartParams (..), startAuction)

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx =    

    case mintOnly of
        True  -> traceIfFalse "UTxO not consumed / cannot mint anymore identical tokens"   hasUTxO  &&
                 traceIfFalse "wrong amount minted" checkMintedAmount
        False -> traceIfFalse "Burn quantity more than existing tokens" checkValidAmount 

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info
    
        getFlattenVal:: [(CurrencySymbol, TokenName, Integer)]
        getFlattenVal = flattenValue (txInfoMint info)
        
        mintOnly :: Bool
        mintOnly = case getFlattenVal of 
            [(_curr, tn', amt')] -> tn' == tn && amt' >= 0  -- assummption: positive amt = minting 
            _                    -> False 
        checkMintedAmount :: Bool
        checkMintedAmount = case getFlattenVal of 
            [(_curr, tn', amt')] -> tn' == tn && amt' == amt
            _                    -> False
        
        checkValidAmount :: Bool
        checkValidAmount = case getFlattenVal of 
            [(_curr, _, amt')] -> amt >= (abs amt') -- burn amt must be less or equal to existng tokens
            _                    -> False
        
tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn
-- tokenCurSymbol oref tn amt = scriptCurrencySymbol  (tokenPolicy oref tn amnt)

-- OFF CHAIN CODE

data TokenParams = TokenParams
    { tpToken   :: !TokenName
    , tpAmount  :: !Integer
    , tpAddress :: !Address
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema) 

data BurnParams = BurnParams
    { btCurrency :: !CurrencySymbol
    , btToken    :: !TokenName
    , btAmount   :: !Integer
    , btAddress  :: !Address
    , btPolicy   :: !TokenPolicyParams  
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

data TokenPolicyParams = TokenPolicyParams
    { tkpOref   :: !TxOutRef
    , tkpToken  :: !TokenName
    , tkpAmount :: !Integer
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

data MintSendParams = MintSendParams
    { msToken    :: !TokenName
    , msDeadline :: !POSIXTime
    , msMinBid   :: !Integer
    , msAddress  :: !Address
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

data ListParams = ListParams
    { lpAddress :: !Address
    , lpWalletName :: !String -- BuiltinByteString
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints
    --ct Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced
    unsigned <- balanceTx unbalanced
    --ct Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned
    --ct Contract.logDebug @String $ printf "signed: %s" $ show signed
    return signed

adjustAndSubmit :: ( PlutusTx.FromData (Scripts.DatumType a)
                   , PlutusTx.ToData (Scripts.RedeemerType a)
                   , PlutusTx.ToData (Scripts.DatumType a)
                   , AsContractError e
                   )
                => Scripts.TypedValidator a
                -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                -> Contract w s e CardanoTx
adjustAndSubmit inst = adjustAndSubmitWith $ Constraints.typedValidatorLookups inst

mintToken :: TokenParams -> Contract w s Text CurrencySymbol
mintToken tp = do
    
    logInfo @P.String "-------------------- Start Minting --------------------"
    
    let addr = tpAddress tp
    case getCredentials addr of   
        Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got %s" $ show addr
        Just (x, my) -> do
            oref <- getUnspentOutput
            o    <- fromJust <$> Contract.txOutFromRef oref
            logInfo @P.String $ printf "picked UTxO at %s" (P.show oref) 
            let tn          = tpToken tp
                amt         = tpAmount tp
                cs          = tokenCurSymbol oref tn amt
                val         = Value.singleton cs tn amt
                c           = case my of
                    Nothing -> Constraints.mustPayToPubKey x val
                    Just y  -> Constraints.mustPayToPubKeyAddress x y val
                lookups     = Constraints.mintingPolicy (tokenPolicy oref tn amt) <>
                              Constraints.unspentOutputs (Map.singleton oref o)
                constraints = Constraints.mustMintValue val          <>
                              Constraints.mustSpendPubKeyOutput oref <>
                              c
            
            ledgerTx <- adjustAndSubmitWith @Void lookups constraints            
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                        
            return cs

burnToken :: BurnParams -> Contract w s Text ()
burnToken bp = do

    logInfo @P.String "-------------------- Burning --------------------"  
    
    utxos <- utxosAt $ (btAddress bp)
       
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , case flattenValue (_ciTxOutValue o) of                    
             -- Note: _citTxOutValue o has 2 components:  (ADA values),(Other native tokens values)                     
                    [(_, _, _), (curr', tn', _')] -> 
                        curr' /= "" && tn' /= "" -- && amt' >= 1  -- extracting other native tokens with value 1 
                    _                                  -> False  
             ]
    case xs of
        []  -> logInfo @P.String "  Token to be Burnt Not Found    "
        ((oref, o) : _)   -> do
            let tn        = btToken bp
                amt       = (btAmount bp) 
                cn        = (btCurrency bp) 
                origamt   = Value.valueOf (_ciTxOutValue o) (btCurrency bp) (btToken bp) 
                tkpparams = btPolicy bp
                tkpolicy  = tokenPolicy (tkpOref tkpparams) (tkpToken tkpparams) (tkpAmount tkpparams)
                -- assuming tokenpolicy structure is fixed as such (above) - user will supply the params
                val       = Value.singleton cn tn amt  
                lookups   = Constraints.mintingPolicy (tkpolicy) <> Constraints.unspentOutputs utxos
                tx        = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            
            logInfo @P.String $ printf "Token Currency: %s " (P.show cn)
            logInfo @P.String $ printf "Token Name: %s " (P.show tn)
            logInfo @P.String $ printf "Current value of Tokens: %s " (P.show origamt)
            logInfo @P.String $ printf "Tokens to be burnt: %s " (P.show (abs amt))
            {--
            Contract.logInfo @String $ printf "Token Currency: %s " (P.show cn)
            Contract.logInfo @String $ printf "Token Name: %s " (P.show tn)
            Contract.logInfo @String $ printf "Current value of Tokens: %s " (P.show origamt)
            Contract.logInfo @String $ printf "Tokens to be burnt: %s " (P.show (abs amt))
            --}

            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx            
            return ()

mintSendToken :: MintSendParams -> Contract w s Text ()
mintSendToken ms = do
    logInfo @P.String "------------------- Mint and Send to Auction --------------------" 
        
    let xs = TokenParams
             { tpToken   = (msToken ms)
             , tpAmount  = 1
             , tpAddress = (msAddress ms) 
             }    
    cs <- mintToken xs  -- mint the required tokens

    case cs of
        "" -> logInfo @P.String "Minting Not Successful" 
        _  -> logInfo @P.String $ printf "The currency symbol is %s" (P.show cs)

    let lp = ListParams
             { lpAddress = (msAddress ms)
             , lpWalletName = "Seller"
             } 

    listTokens lp

    let sp = StartParams
            { spDeadline = (msDeadline ms)
            , spMinBid   = (msMinBid ms)
            , spCurrency = cs
            , spToken    = (msToken ms)
            } 
 
    startAuction sp
    return ()

listTokens :: ListParams -> Contract w s Text ()
listTokens lp = do
    logInfo @P.String $ printf "-------------------- Listing for %s --------------------" (lpWalletName lp)
    utxos <- utxosAt (lpAddress lp)
    -- logInfo @P.String $ printf "------- UTXOS for %s -------" (P.show utxos)
    
    let xs = [ flattenValue (_ciTxOutValue o) 
             | (_, o) <- Map.toList utxos        
             , case flattenValue (_ciTxOutValue o) of
                    [(_, _, _), (curr', tn', _)] -> curr' /= "" && tn' /= "" -- && amt' == 1  -- extracting other native tokens with value 1 
                    _                                  -> False       
             ]
    case xs of
        [] -> logInfo @P.String "No NFTs acquired" 
        _  -> logInfo @P.String $ printf "Current token(s):  %s" (P.show xs)

-- Endpoints Schema ---

{-- Commented off - defined in CtSchema.hs 
type MintSchema = 
        Endpoint "mint" TokenParams

endpoints1 :: Contract () MintSchema Text ()
endpoints1 = mint' >> endpoints2
  where
    mint' = awaitPromise $ endpoint @"mint" mintToken

type BurnSendListSchema = 
        Endpoint "list" ListParams
    .\/ Endpoint "mintsend" MintSendParams
    .\/ Endpoint "mint" TokenParams
    .\/ Endpoint "burn" BurnParams

endpoints2 :: Contract () BurnSendListSchema Text ()
endpoints2 = awaitPromise (list' `select` mintsend' `select` burn' ) >> endpoints2
  where
    list'     = endpoint @"list"  listTokens
    mintsend' = endpoint @"mintsend" mintSendToken
    burn'     = endpoint @"burn" burnToken
--}