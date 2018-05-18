const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');

test.only('Exported methods are as indicated', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/exports.dia' });

  // web3 adds a whole lot of alias names that we want to filter out
  const exportedMethods = Object.keys(contract.methods).filter(name => !name.startsWith('0x') && !name.includes('()'));
  const expectedMethods = ['fun_1', 'fun_2', 'fun_3'];
  t.deepEqual(
    exportedMethods.length,
    3,
  );

  expectedMethods.forEach(methodName => t.true(exportedMethods.includes(methodName)));
});
