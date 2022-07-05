{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Apps.CtSchema where

import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Plutus.Contract             as Contract
import           Data.Text                   (Text) 

import           Apps.CtMintShop
import           Apps.CtMarketPlace
import           Apps.CtOracleCore
import           Apps.CtOracleSwap

-- Endpoints Schema ---

type MintSchema = 
        Endpoint "mint" TokenParams

endpoints1 :: Contract () MintSchema Text ()
endpoints1 = mint' >> endpoints1
  where
    mint' = awaitPromise $ endpoint @"mint" mintToken

type ProjectSchema = 
    Endpoint "start" StartParams
    .\/ Endpoint "bid"   BidParams
    .\/ Endpoint "close" CloseParams
    .\/ Endpoint "list" ListParams
    .\/ Endpoint "mintsend" MintSendParams
    .\/ Endpoint "mint" TokenParams
    .\/ Endpoint "burn" BurnParams

endpoints2 :: Contract () ProjectSchema Text ()
endpoints2 = awaitPromise (start' `select` bid' `select` close' `select` list' `select` mintsend' `select` burn' ) >> endpoints2
  where
    start'    = endpoint @"start" startAuction
    bid'      = endpoint @"bid"   bidForItem
    close'    = endpoint @"close" closeAuction
    list'     = endpoint @"list"  listTokens
    mintsend' = endpoint @"mintsend" mintSendToken
    burn'     = endpoint @"burn" burnToken
