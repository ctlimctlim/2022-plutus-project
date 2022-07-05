{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores #-}

module Apps.CtOracleCore
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleValue
    , oracleAsset
    , oracleInst
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , findOracle
    , runOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, Maybe (Nothing))
import           Data.Either               (fromRight)
import           Data.Monoid               (Last (..),(<>))
import           Data.Text                 (Text, pack)
import           Data.Void                 (Void)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import qualified Plutus.Contracts.Currency as Currency
import           Prelude                   (Show, show, String)
import qualified Prelude                   as Prelude
import qualified Prelude                   as P
import qualified PlutusTx
import qualified Plutus.V2.Ledger.Api      as PlutusTx
import           Plutus.V1.Ledger.Value    (currencySymbol)
import qualified Plutus.V2.Ledger.Api      as PlutusTx
import           Text.Printf               (printf)

data Oracle = Oracle
    { oOracleSymbol :: !CurrencySymbol -- NFT identifiying the Orcale
    , oOracleTokenName :: !TokenName
    , oOperator :: !PaymentPubKeyHash
    , oFee      :: !Integer
    , oCurrency :: !CurrencySymbol -- asset that wants to be exchanged
    , oToken    :: !TokenName
    , oExchange :: !Integer        -- exchange rate
    } deriving (Show, Generic, FromJSON, ToJSON)

instance Eq Oracle where
    {-# INLINABLE (==) #-}
    a == b = oOracleSymbol a == oOracleSymbol b &&
             oOracleTokenName a == oOracleTokenName b &&
             oOperator a == oOperator b &&
             oFee a == oFee b &&
             oCurrency a == oCurrency b &&
             oToken a == oToken b &&
             oExchange a == oExchange b

instance Ord Oracle where
    {-# INLINABLE (<=) #-}
    a <= b = True

PlutusTx.unstableMakeIsData ''Oracle
PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oOracleSymbol oracle, oOracleTokenName oracle)

{-# INLINABLE oracleDatum #-}
oracleDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Oracle
oracleDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = oExchange <$> oracleDatum o f

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle r ctx = 
    traceIfFalse "oracle Nft missing from input"  inputHasToken  &&
    traceIfFalse "oracle Nft missing from output" outputHasToken &&
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ unPaymentPubKeyHash $ oOperator oracle) &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just oracle)         &&
                  traceIfFalse "fees not paid"              feesPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Oracle
    outputDatum = oracleDatum ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid = 
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Oracle
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Scripts.TypedValidator  Oracling
oracleInst = Scripts.mkTypedValidator @Oracling
    $$(PlutusTx.compile [|| mkOracleValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Oracle @OracleRedeemer

oracleValidator :: Validator
oracleValidator = Scripts.validatorScript oracleInst

oracleAddress ::  Ledger.Address
oracleAddress = scriptAddress oracleValidator

-- OFF CHAIN CODE

data OracleParams = OracleParams
    { opOracleSymbol :: !CurrencySymbol -- NFT identifiying the Orcale
    , opOracleTokenName :: !TokenName
    --, opAddress :: !Address
    , opFee    :: !Integer
    , opSymbol :: !CurrencySymbol
    , opToken  :: !TokenName
    , opExchange :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s e. AsContractError e => OracleParams -> Contract (Last Oracle) s e Oracle
startOracle op = do
    pkh <- Contract.ownPaymentPubKeyHash
    let oracle = Oracle
            { oOracleSymbol = opOracleSymbol op
            , oOracleTokenName  = opOracleTokenName op
            , oOperator = pkh
            , oFee      = opFee op
            , oCurrency = opSymbol op
            , oToken = opToken op
            , oExchange = opExchange op
            }
    --20220628 commented: logInfo @String $ "started oracle " ++ show oracle
    logInfo @P.String $ printf "-------------------- Starting Oracle --------------------" 
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s e. AsContractError e => Oracle -> Integer -> Contract (Last Oracle) s e ()
updateOracle oracle ex = do
    logInfo @P.String $ printf "-------------------- Updating Oracle --------------------" 
    m <- findOracle oracle
    let
      oracleNew = oracle { oExchange = ex }
      -- assumption: the wallet already own the oracle NFT
      c = Constraints.mustPayToTheScript oracleNew $ assetClassValue (oracleAsset oracle) 1 <> lovelaceValueOf 2_000_000
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints oracleInst c
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show ex
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups oracleInst <>
                          Constraints.otherScript oracleValidator
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            --ledgerTx <- adjustAndSubmitWith @Oracling lookups tx
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show ex
    tell $ Last $ Just oracleNew

findOracle :: forall w s e. AsContractError e => Oracle -> Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, Oracle))
findOracle oracle = do
    utxos <- Map.filter f <$> Contract.utxosAt oracleAddress
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            d <- oracleDatum (toTxOut  o) $ \dh -> case _ciTxOutDatum o of
                                                     Left _ -> Nothing
                                                     Right d -> Just d
            return (oref, o, d)
        _           -> Nothing
  where
    f :: ChainIndexTxOut -> Bool
    f o = assetClassValueOf (_ciTxOutValue o) (oracleAsset oracle) == 1

type OracleSchema = Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = awaitPromise (update oracle) >> go oracle

    update oracle = endpoint @"update" $ updateOracle oracle

