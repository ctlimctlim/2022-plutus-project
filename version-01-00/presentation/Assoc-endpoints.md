## ASSOCIATED ENDPOINTS AND FUNCTIONS

* SCENE 1 - Pre-Allocation of 100_000_000 USDT and 10_000 lovelace to the respective knownwallets 1-4

    **functions invoked:** MyTrace () from (PlutusTestvXX.hs)

* SCENE 2 - Mint the Oracle NFT for Wallet 1

    **endpoints invoked:** "mint"(mintToken), "list"(listTokens) - CtMintShop.hs

* SCENE 3 - Start the Oracle by Wallet 1!!!!

    **functions invoked:** runOracle - CtOracleCore.hs, getOracle - PlutusTestvxx.hs

* SCENE 4 - Oracle (Wallet 1) initialise its first exchange rate

    **endpoints(fn) invoked:** "update"(updateOracle) - CtOracleCore.hs

* SCENE 5 - Wallet 4 wants some ADA in exchange for USDT - Let us do some swap

    **endpoints(fn) invoked:** "offer"(offer), "use"(use) - CtOracleSwap.hs

* SCENE 6 - Wallet 1 to update its Oracle to raise its exchange rate

    **endpoints(fn) invoked:** "update"(updateOracle) - CtOracleCore.hs

* SCENE 7 - Wallet 4 now wants to create NFt and sent for Auction immediately

    **endpoints(fn) invoked:** "mintsend"(mintSendToken) - CtMintShop.hs

* SCENE 8 - Let the bidding begins between Wallets 1,2,3

    **endpoints(fn) invoked:** "bid"(bid), "close"(close) - CtMarketPlace.hs

* SCENE 9 - Wallet 2 wants to mint Tokens and decide to create 8 identical ones

    **endpoints invoked:** "mint"(mintToken), "list"(listTokens) - CtMintShop.hs

* SCENE 10 - Wallet 2 somehow thinks that having 8 identical tokens are too many, decides to burn some (6)

    **endpoints invoked:** "burn"(burnToken) - CtMintShop.hs

* SCENE 11 - Wallet 2 decides to finally to reduce the token value to 1

    **endpoints invoked:** "burn"(burnToken) - CtMintShop.hs

* SCENE 12 - Wallet 2 finally decides to send his only token to the auction

    **endpoints(fn) invoked:** "start"(startAuction) - CtMarketPlace.hs

* SCENE 13 - Wallet 3 wants to try his luck at auction again... and the bidding ensues...

    **endpoints(fn) invoked:** "bid"(bid) - CtMarketPlace.hs