const recipientAddress = '0x2c085463e2d726cbb98e1222c89506e82d90890e';

module.exports = {
  contract: ['simple_token', 'SimpleToken'],
  methodTests: [
    {
      name: 'balance',
      create: ({ contract, coinbase }) => contract.methods.balance(coinbase),
      test: ({ result }) => result === '1000000',
    },
    {
      name: 'send',
      create: ({ contract, coinbase }) => contract.methods.transfer(recipientAddress, '50'),
    },
  ],
};
