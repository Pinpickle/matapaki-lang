const { extraAccountAddress } = require('../../utils/blockchain');

module.exports = {
  contract: ['open_auction', 'OpenAuction'],
  methodTests: [
    {
      name: 'bid',
      value: 10,
      create: ({ contract, coinbase }) => Object.assign(
        contract.methods.bid(),
        { valueToSend: 10 },
      ),
      test: async ({ contract, coinbase }) => (
        ((await contract.methods.highest_bidder().call({ from: coinbase, gas: 400000 })).toLowerCase() === coinbase.toLowerCase()) &&
        (await contract.methods.highest_bid().call({ from: coinbase, gas: 400000 }) === '10')
      ),
    },
    {
      name: 'second bid',
      value: 20,
      create: async ({ contract, coinbase }) => {
        await contract.methods.bid().call({ from: extraAccountAddress, gas: 400000, value: 10 });
        return contract.methods.bid();
      },
      test: async ({ contract, coinbase }) => (
        ((await contract.methods.highest_bidder().call({ from: coinbase, gas: 400000 })).toLowerCase() === coinbase.toLowerCase()) &&
        (await contract.methods.highest_bid().call({ from: coinbase, gas: 400000 }) === '20')
      ),
    },
    {
      name: 'withdraw after bid',
      create: async ({ contract, coinbase }) => {
        await contract.methods.bid().send({ from: coinbase, gas: 400000, value: 10 });
        await contract.methods.bid().send({ from: coinbase, gas: 400000, value: 20 });
        
        return contract.methods.withdraw();
      },
      test: async ({ contract, coinbase, client }) => (
        await client.eth.getBalance(contract.options.address) === '20'
      ),
    },
    {
      name: 'empty withdraw',
      create: ({ contract }) => contract.methods.withdraw(),
    },
    {
      name: 'end auction',
      create: async ({ contract, coinbase }) => {
        await contract.methods.bid().call({ from: extraAccountAddress, gas: 400000, value: 10 });
        await contract.methods.bid().call({ from: extraAccountAddress, gas: 400000, value: 20 });

        return contract.methods.end_auction();
      },
      test: async ({ contract, coinbase, client }) => (
        await client.eth.getBalance(contract.options.address) === '0'
      )
    }
  ],
};
