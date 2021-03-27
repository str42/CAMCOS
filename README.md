# CAMCOS
Scripts for Camcos Project
Pending Transactions_Confirmations is the code randomly select 500 transactions and merge that information with summary information about the block and the number of pending transactions in the mempool.  Number of transactions in the mempool is the three day .csv download from etherscan.io 

Random: Generates one random transactions per hour from a list of transactions

SummaryScript: Takes transactions hash, block#, gas amt, gas price and summarizes information by block

scrapernew: input transaction hash output confirmation time, block#, day/time, gasamt, gasprice, total fee (This is very slow due as the script must wait 21 seconds every 29 transactions to keep from getting booted out of etherscan.

convert_seconds: takes the time from confirmation time and converts it all into seconds
