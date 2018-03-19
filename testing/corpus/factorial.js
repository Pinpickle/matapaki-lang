module.exports = {
  contract: ['factorial', 'Factorial'],
  methodTests: [
    {
      name: 'low factorial',
      create: ({ contract }) => contract.methods.factorial(3),
      test: ({ result }) => result === '6',
    },
    {
      name: 'high factorial',
      create: ({ contract }) => contract.methods.factorial(30),
      test: ({ result }) => result === '265252859812191058636308480000000',
    }
  ],
};
