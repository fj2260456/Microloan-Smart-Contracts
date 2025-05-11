# Microloan Smart Contract

A trustless lending system built on Stacks blockchain that enables peer-to-peer microloans with collateral or social proof.

## Overview

This smart contract implements a decentralized microloan platform where users can:

- Create loan requests with collateral
- Fund loans as a lender
- Repay loans as a borrower
- Claim collateral in case of default
- Build reputation through successful loan repayments
- Endorse other users to build social trust

## Contract Features

- **Collateralized Loans**: Borrowers must provide sufficient collateral to create a loan
- **Reputation System**: Tracks successful and defaulted loans to build user reputation
- **Social Endorsements**: Allows users to vouch for others with trust scores
- **Configurable Parameters**: Platform fee and minimum collateral ratio can be adjusted

## How to Use

### For Borrowers

1. **Create a Loan Request**:
   ```
   (contract-call? .microloan create-loan amount collateral duration interest-rate)
   ```
   - `amount`: The loan amount requested (in microSTX)
   - `collateral`: The collateral amount provided (in microSTX)
   - `duration`: Loan duration in blocks
   - `interest-rate`: Annual interest rate (0-100)

2. **Repay a Loan**:
   ```
   (contract-call? .microloan repay-loan loan-id)
   ```

3. **Cancel a Pending Loan**:
   ```
   (contract-call? .microloan cancel-loan loan-id)
   ```

### For Lenders

1. **Fund a Loan**:
   ```
   (contract-call? .microloan fund-loan loan-id)
   ```

2. **Claim Collateral for Defaulted Loan**:
   ```
   (contract-call? .microloan claim-collateral loan-id)
   ```

### Social Trust

1. **Endorse Another User**:
   ```
   (contract-call? .microloan endorse-user user-address trust-score)
   ```
   - `trust-score`: A value between 0-100 indicating your trust level

2. **Check User Reputation**:
   ```
   (contract-call? .microloan get-reputation user-address)
   ```

## Contract Parameters

- **Platform Fee**: 5% (configurable by contract owner)
- **Minimum Collateral Ratio**: 150% (configurable by contract owner)

## Loan Statuses

- `PENDING`: Loan created but not yet funded
- `ACTIVE`: Loan funded and currently active
- `REPAID`: Loan successfully repaid
- `DEFAULTED`: Loan defaulted, collateral claimed
- `CANCELLED`: Loan cancelled before funding

## Development

This contract is built for the Stacks blockchain using Clarity language and can be deployed using Clarinet.
