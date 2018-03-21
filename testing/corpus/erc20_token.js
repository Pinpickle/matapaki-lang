const { extraAccountAddress } = require('../utils/blockchain');

module.exports = {
  contract: ['erc20_token', 'ERC20Token'],
  methodTests: [
    {
      name: 'balanceOf',
      create: ({ contract, coinbase }) => contract.methods.balanceOf(coinbase),
      test: ({ result }) => result === '100000',
    },
    {
      name: 'transfer',
      create: ({ contract, coinbase }) => contract.methods.transfer(extraAccountAddress, 10),
      test: async ({ result, contract, coinbase }) => (
        result === true &&
        await contract.methods.balanceOf(coinbase).call({ from: coinbase, gas: 400000 }) === '99990' &&
        await contract.methods.balanceOf(extraAccountAddress).call({ from: coinbase, gas: 400000 }) === '10'
      ),
    },
    {
      name: 'approve',
      create: ({ contract, coinbase }) => contract.methods.approve(extraAccountAddress, 10),
      test: async ({ result, contract, coinbase }) => (
        result === true &&
        await contract.methods.allowance(coinbase, extraAccountAddress).call({ from: coinbase, gas: 400000 }) === '10'
      ),
    },
    {
      name: 'allowance',
      create: async ({ contract, coinbase }) => {
        await contract.methods.approve(extraAccountAddress, 10).send({ from: coinbase, gas: 400000 });
        return contract.methods.allowance(coinbase, extraAccountAddress);
      },
      test: async ({ result }) => result === '10',
    },
    {
      name: 'transferFrom',
      create: async ({ contract, coinbase }) => {
        await contract.methods.transfer(extraAccountAddress, 10).send({ from: coinbase, gas: 400000 })
        await contract.methods.approve(coinbase, 10).send({ from: extraAccountAddress, gas: 400000 });
        return contract.methods.transferFrom(extraAccountAddress, coinbase, 5);
      },
      test: async ({ result, contract, coinbase }) => (
        result === true &&
        await contract.methods.balanceOf(coinbase).call({ from: coinbase, gas: 400000 }) === '99995' &&
        await contract.methods.balanceOf(extraAccountAddress).call({ from: coinbase, gas: 400000 }) === '5' &&
        await contract.methods.allowance(extraAccountAddress, coinbase).call({ from: coinbase, gas: 400000 }) === '5'/**/
      ),
    },
  ],
};
