const Web3 = require('web3');
const ganache = require('ganache-cli');

function createBlockchainClient() {
  return new Web3(ganache.provider({
    gasLimit: 900000000000000,
    gasPrice: 1,
    accounts: [
      {
        balance: 1000000000000000000000000,
      },
    ],
  }));
}

module.exports = {
  createBlockchainClient,
};
