## TEST RUN (Complete) OF "testWorkFlow" (PlutusTestv10.hs)

Prelude PlutusTestv10clean> testWorkFlow
Slot 00001: *** USER LOG: Wallets: ["Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491","Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58","Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154","Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8","Wallet d3eddd0d37989746b029a0e050386bc425363901","Wallet 4e76ce6b3f12c6cc5a6a2545f6770d2bcb360648","Wallet 1bc5f27d7b4e20083977418e839e429d00cc87f3","Wallet 3a4778247ad35117d7c3150d194da389f3148f4a","Wallet c19599f22890ced15c6a87222302109e83b78bdf","Wallet bdf8dbca0cadeb365480c6ec29ec746a2b85274f"]

Slot 00001: *** CONTRACT LOG: "-------------------- Start Minting --------------------"

Slot 00001: *** CONTRACT LOG: "picked UTxO at TxOutRef {txOutRefId = 5f920566627190ccf354f3c5da52d601f5fc44bfe371f940c0dc57c1c9ccca81, txOutRefIdx = 3}"

Slot 00002: *** CONTRACT LOG: "-------------------- Listing for Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491 --------------------"

Slot 00002: *** CONTRACT LOG: "Current token(s):  [[(,\"\",2000000),(06d31dbc1f726f6eda3d54e5b98134cadba0bc5775d5a5596cfe0f9e,\"OracleNFT\",1)]]"

Slot 00004: *** CONTRACT LOG: "-------------------- Starting Oracle --------------------"

Slot 00004: *** CONTRACT LOG: "started oracle Oracle {oOracleSymbol = 06d31dbc1f726f6eda3d54e5b98134cadba0bc5775d5a5596cfe0f9e, oOracleTokenName = \"OracleNFT\", oOperator = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, oFee = 1000000, oCurrency = ff, oToken = \"USDT\", oExchange = 2}"

Slot 00006: *** USER LOG: Oracle {oOracleSymbol = 06d31dbc1f726f6eda3d54e5b98134cadba0bc5775d5a5596cfe0f9e, oOracleTokenName = "OracleNFT", oOperator = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2, oFee = 1000000, oCurrency = ff, oToken = "USDT", oExchange = 2}

Slot 00006: *** CONTRACT LOG: "-------------------- Updating Oracle --------------------"

Slot 00007: *** CONTRACT LOG: "set initial oracle value to 2"

Slot 00009: *** CONTRACT LOG: "-------------------- Offering to Swap --------------------"

Slot 00010: *** CONTRACT LOG: "offered 10000000 lovelace for swap"

Slot 00010: *** CONTRACT LOG: "-------------------- Accepting Offer to Swap --------------------"

Slot 00010: *** CONTRACT LOG: "available assets: 20"

Slot 00010: *** CONTRACT LOG: "found oracle, exchange rate 2"

Slot 00010: *** CONTRACT LOG: "found suitable swap"

Slot 00011: *** CONTRACT LOG: "made swap with price [(,\"\",2000000),(ff,\"USDT\",20)]"

Slot 00013: *** CONTRACT LOG: "-------------------- Updating Oracle --------------------"

Slot 00014: *** CONTRACT LOG: "updated oracle value to 3"

Slot 00016: *** CONTRACT LOG: "------------------- Mint and Send to Auction --------------------"

Slot 00016: *** CONTRACT LOG: "-------------------- Start Minting --------------------"

Slot 00016: *** CONTRACT LOG: "picked UTxO at TxOutRef {txOutRefId = b80d3d64bb125c199c303b88b3bbc8e84e9f9bf7078b005fc42679839e08657c, txOutRefIdx = 0}"

Slot 00017: *** CONTRACT LOG: "The currency symbol is fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d"

Slot 00017: *** CONTRACT LOG: "-------------------- Listing for Seller --------------------"

Slot 00017: *** CONTRACT LOG: "Current token(s):  [[(,\"\",2000000),(fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d,\"W4_NFT\",1)]]"

Slot 00017: *** CONTRACT LOG: "-------------------- Starting Auction --------------------"

Slot 00020: *** CONTRACT LOG: "-------------------- Making a Bid --------------------"

Slot 00020: *** CONTRACT LOG: "found auction utxo with datum AuctionDatum {adAuction = Auction {aSeller = 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d, aDeadline = POSIXTime {getPOSIXTime = 1596059101000}, aMinBid = 15000000, aCurrency = fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d, aToken = \"W4_NFT\"}, adHighestBid = Nothing}"

Slot 00022: *** CONTRACT LOG: "-------------------- Making a Bid --------------------"

Slot 00022: *** CONTRACT LOG: "found auction utxo with datum AuctionDatum {adAuction = Auction {aSeller = 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d, aDeadline = POSIXTime {getPOSIXTime = 1596059101000}, aMinBid = 15000000, aCurrency = fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d, aToken = \"W4_NFT\"}, adHighestBid = Nothing}"

Slot 00022: *** CONTRACT STOPPED WITH ERROR: "\"bid lower than minimal bid 15000000\""

Slot 00024: *** CONTRACT LOG: "-------------------- Making a Bid --------------------"

Slot 00024: *** CONTRACT LOG: "found auction utxo with datum AuctionDatum {adAuction = Auction {aSeller = 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d, aDeadline = POSIXTime {getPOSIXTime = 1596059101000}, aMinBid = 15000000, aCurrency = fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d, aToken = \"W4_NFT\"}, adHighestBid = Nothing}"

Slot 00034: *** CONTRACT LOG: "-------------------- Closing Auction --------------------"

Slot 00034: *** CONTRACT LOG: "found auction utxo with datum AuctionDatum {adAuction = Auction {aSeller = 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d, aDeadline = POSIXTime {getPOSIXTime = 1596059101000}, aMinBid = 15000000, aCurrency = fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d, aToken = \"W4_NFT\"}, adHighestBid = Nothing}"

Slot 00035: *** CONTRACT LOG: "closed auction Auction {aSeller = 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d, aDeadline = POSIXTime {getPOSIXTime = 1596059101000}, aMinBid = 15000000, aCurrency = fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d, aToken = \"W4_NFT\"} for token (fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d, \"W4_NFT\")"

Slot 00035: *** CONTRACT LOG: "-------------------- Start Minting --------------------"

Slot 00035: *** CONTRACT LOG: "picked UTxO at TxOutRef {txOutRefId = 5f920566627190ccf354f3c5da52d601f5fc44bfe371f940c0dc57c1c9ccca81, txOutRefIdx = 2}"

Slot 00038: *** CONTRACT LOG: "-------------------- Listing for Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58 --------------------"

Slot 00038: *** CONTRACT LOG: "Current token(s):  [[(,\"\",2000000),(b6ce6e35ecd1d1bf7c7641bc46f6de3692c9e122897eab9a7dbed4e3,\"W2_Tokens\",8)]]"

Slot 00039: *** CONTRACT LOG: "-------------------- Burning --------------------"

Slot 00039: *** CONTRACT LOG: "Token Currency: b6ce6e35ecd1d1bf7c7641bc46f6de3692c9e122897eab9a7dbed4e3 "

Slot 00039: *** CONTRACT LOG: "Token Name: \"W2_Tokens\" "

Slot 00039: *** CONTRACT LOG: "Current value of Tokens: 8 "

Slot 00039: *** CONTRACT LOG: "Tokens to be burnt: 6 "

Slot 00040: *** CONTRACT LOG: "-------------------- Burning --------------------"

Slot 00040: *** CONTRACT LOG: "Token Currency: b6ce6e35ecd1d1bf7c7641bc46f6de3692c9e122897eab9a7dbed4e3 "

Slot 00040: *** CONTRACT LOG: "Token Name: \"W2_Tokens\" "

Slot 00040: *** CONTRACT LOG: "Current value of Tokens: 2 "

Slot 00040: *** CONTRACT LOG: "Tokens to be burnt: 1 "

Slot 00043: *** CONTRACT LOG: "-------------------- Starting Auction --------------------"

Slot 00044: *** CONTRACT LOG: "-------------------- Making a Bid --------------------"

Slot 00044: *** CONTRACT LOG: "found auction utxo with datum AuctionDatum {adAuction = Auction {aSeller = 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7, aDeadline = POSIXTime {getPOSIXTime = 1596059101000}, aMinBid = 5000000, aCurrency = b6ce6e35ecd1d1bf7c7641bc46f6de3692c9e122897eab9a7dbed4e3, aToken = \"W2_Tokens\"}, adHighestBid = Nothing}"

Final balances
Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8:
    {fca87d1df896c2b096ee965adb29e3bd1d536232ad1285d9474cb53d, "W4_NFT"}: 1
    {ff, "USDT"}: 9980
    {, ""}: 106976329
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58:
    {ff, "USDT"}: 10000
    {, ""}: 97989154
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491:
    {ff, "USDT"}: 10000
    {, ""}: 98990224
Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154:
    {, ""}: 91999990
    {ff, "USDT"}: 10020
Script 0800eb2f668677ed8b3172ac67a47ecdafc0922cce133c5777540d74:
    {, ""}: 2000000
    {06d31dbc1f726f6eda3d54e5b98134cadba0bc5775d5a5596cfe0f9e, "OracleNFT"}: 1
Script 5ebfe65f500d3ce36f101e569217a4ed351ddeffd638b2be42fd9650:
    {, ""}: 2000000
    {b6ce6e35ecd1d1bf7c7641bc46f6de3692c9e122897eab9a7dbed4e3, "W2_Tokens"}: 1
Prelude PlutusTestv10clean> 