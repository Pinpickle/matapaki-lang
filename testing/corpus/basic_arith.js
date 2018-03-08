module.exports = {
  contract: ['basic_arith', 'BasicArith'],
  methodTests: [
    {
      name: 'addition',
      create: ({ contract }) => contract.methods.add(5, 10),
      test: ({ result }) => result === '15',
    },
    {
      name: 'subtraction',
      create: ({ contract }) => contract.methods.subtract(5, 10),
      test: ({ result }) => result === '-5',
    },
  ],
};
