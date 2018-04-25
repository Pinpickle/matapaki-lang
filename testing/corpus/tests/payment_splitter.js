const { extraAccountAddress } = require('../../utils/blockchain');

module.exports = {
  contract: ['payment_splitter', 'PaymentSplitter'],
  methodTests: [
    {
      name: 'set second account',
      create: ({ contract }) => contract.methods.set_account_2(extraAccountAddress),
    },
    {
      name: 'pay',
      create: async ({ contract, coinbase }) => {
        await contract.methods.set_account_2(extraAccountAddress).send({ from: coinbase, gas: 400000 });
        return contract.methods.pay()
      },
      value: 1000,
    },
    {
      name: 'claim balance',
      create: async ({ contract, coinbase }) => {
        await contract.methods.set_account_2(extraAccountAddress).send({ from: coinbase, gas: 400000 });
        await contract.methods.pay().send({ from: coinbase, gas: 400000, value: 1000 });
        return contract.methods.claim_balance();
      },
      test: ({ result }) => result === '500',
    },
    {
      name: 'claim balance twice',
      create: async ({ contract, coinbase }) => {
        await contract.methods.set_account_2(extraAccountAddress).send({ from: coinbase, gas: 400000 });
        await contract.methods.pay().send({ from: coinbase, gas: 400000, value: 1000 });
        await contract.methods.claim_balance().send({ from: coinbase, gas: 400000 });
        return contract.methods.claim_balance();
      },
      test: ({ result }) => result === '0',
    },
  ],
};
