## Project Description
- Title: NFT, Tokens and MarketPlace
- Last updated: 1 July 2022 (version 1.0)

## Overview

- This project is submitted as part of the final deliverables of the Cardano Development Professional course (Batch 37). The project demonstrates the workflow of the user attempting to buy ADA using USDT from which he/she minted its own NFT in the Cardano Blockchain and then sending it to the marketplace, in this particular instance - an auction facility, for sale to the highest bidder.  This project also features the minting of multiple identical tokens and eventually the burning of the same tokens. 

- The **primary objective** of this project is to demonstrate the understanding and application of the various course subjects namely:
    i)      Structure and Development in Haskell/Plutus
    ii)     Oracles, minting (burning), auction
    iii)    Integration of the various modules into a single dapps where the various functions/ endpoints 
            are utlised to perform a specified cycle of the creation and sale of NFTs.
    iv)     Testing of the workflow using the same set of wallets from start to end of the workflow

- The below gives an graphical overview of the project itself..

![alt text](https://github.com/ctlimctlim/2022-plutus-project/blob/master/version-01-00/presentation/Slide1.jpg?raw=true)


## Project Files
- cdp2022-plutus-project.cabal
- src/PlutsTestv10clean.hs    - main execution file (testWorkFlow :: IO ())
- src/Apps/CtMintShop.hs      - functions relating to minting, burning, listing of NFT and Tokens
- src/Apps/CtMarketPlace.hs   - functions relating to auctions 
- src/Apps/CtOracleCore.hs    - functions relating to starting and updating of Oracle  
- src/Apps/CtOracleSwap.hs    - functions relating to 
- src/Apps/CtSchema.hs        - definitions of schemas and endpoints

* NOTE: the project contains are developed from scratch and codes that are modified from previous codes that made available in the course itself. See under section: - ASSOCIATED ENDPOINTS / FUNCTIONS  
https://github.com/ctlimctlim/2022-plutus-project/tree/master/version-01-00#associated-endpoints--functions

## Test Plan:
A single "integrated" emulator trace test plan consisting of **13** "test scenarios" is formulated to demonstrate a start to end workflow of an NFT sale.

### Integrated Test Scenarios 

- SCENE 01 - Pre-Allocation of 100_000_000 USDT and 10_000 lovelace to 
			the respective knownwallets 1-4 (aka John, Mary, Peter, Jane) 

- SCENE 02 - Mint the Oracle NFT for John (W1) 

- SCENE 03 - Start the Oracle by John (W1)!!!!

- SCENE 04 - Oracle John (W1) initialise its first exchange rate

- SCENE 05 - Jane (W4) wants some ADA in exchange for USDT - Let's do some swap!!!

- SCENE 06 - John (W1) to update its Oracle to raise its exchange rate

- SCENE 07 - Jane (W4) now wants to create NFt and sent for Auction immediately

- SCENE 08 - Let the bidding begins amongst John, Mary, Peter (W1, W2, W3) 

- SCENE 09 - Mary (W2) wants to mint Tokens and decide to create 8 identical ones 

- SCENE 10 - Mary (W2) somehow thinks that having 8 identical tokens are too many, decides to burn some (6)

- SCENE 11 - Mary (W2) decides to finally to reduce the token value to 1

- SCENE 12 - Mary (W2) finally decides to send her only token to the auction

- SCENE 13 - Peter (W3) wants to try his luck at auction again... and the bidding ensues...


* To run the emulator trace tests, run in cabal repl, load src/PlutusTestv10.hs and run testWorkFlow.

## Associated endpoints / functions

The following are the endpoints/functions that facilitate the exexcution of the integrated scenarios: 

https://github.com/ctlimctlim/cdp-final-project/blob/master/cdp2022-plutus-project-v01/presentation/Assoc-endpoints.md

## Test Run Outcome

The documentation of that out come can be found in the below link.  In order to see that each scenarios are properly and accurately executed, the test was "broken up then integrating the next scenario" so that the changes of wallets items resulting from each subseuquent scenrios can be oberved and analysed.

https://github.com/ctlimctlim/2022-plutus-project/blob/master/version-01-00/presentation/PlutusTestv10outcome-breakdown.md

The complete run of the program and the outcome can be found here:

https://github.com/ctlimctlim/2022-plutus-project/blob/master/version-01-00/presentation/PlutusTestv10outcome-full.md

## Future enhancements and final thoughts

### Future enhancments

1. As a formal submission, a version 1.0 (which is at this point of writing) of the project is created.  However continual enhancements are being made to this project and will be added on as an when it is available and prior to its assessment.  Any addition will be documented with a higer version number.

2. The suggested enahancement to this project includes:
    * furhter testing of the current verison of the project by using more robust test cases.
    * creating another oracle to reflect the swap from ADA to USDT or other stable currencies.
    * creating a feature whereby an interested party is able to initiate an fixed offer to an owner of an NFT/Tokens and for the owner to accept the offer.
    * creating a barter trade facility for NFTs owners to swap their tokens
    * creating a wholesale marketplace where a seller can offer a set number of tokens to a buyer
    * a consignment facility whereby seller can consign an set of toekns to a potential 'agent' for a fixed period in which "unsold" tokens will be returned to seller perhaps with a formualted consignment fee.

### Final thoughts on the project

1. It has been a very steep curve in learning to code in Haskell and Plutus.  As such, a substantial amount of time is used to perform and correct the coding itself rather than to design a more innovative approach to the problem at hand.

2. When it was suggested, I decided to take up on this particular project theme as it enables me to review the various modules that were taught in the course namely - oracle, mint and transactions. It aslo helps me to relate to existing live Cardano NFT/Auction from a plutus perspective. 

3. While the processes of swap, NFT minting and auction are already quite well defined, through this exercise, these functions and the suggested future enhancements can be eventually applied in other domains - than just image NFTs.
