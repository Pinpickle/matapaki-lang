const { extraAccountAddress } = require('../../utils/blockchain');

module.exports = {
  contract: ['binary_vote', 'BinaryVote'],
  methodTests: [
    {
      name: 'yes vote',
      create: ({ contract }) => contract.methods.vote(true),
    },
    {
      name: 'end vote',
      create: ({ contract }) => contract.methods.end_vote(),
    },
    {
      name: 'get winner',
      create: async ({ contract, coinbase }) => {
        await contract.methods.vote(true).send({ from: coinbase, gas: 400000 });
        await contract.methods.vote(true).send({ from: extraAccountAddress, gas: 400000 });
        await contract.methods.end_vote().send({ from: coinbase, gas: 400000 });
        return contract.methods.get_winner();
      },
      test: ({ result }) => result === true,
    },
  ],
};
