const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');

test('Single calls', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/functions.dia' });

  t.deepEqual(
    await contract.methods.single_call().call({ from: coinbase, gas: 40000 }),
    '5',
  );
});

test('Wide calls', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/functions.dia' });

  t.deepEqual(
    await contract.methods.wide_call(10).call({ from: coinbase, gas: 40000 }),
    '22',
  );
});

test('Deep calls', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/functions.dia' });

  t.deepEqual(
    await contract.methods.deep_call().call({ from: coinbase, gas: 40000 }),
    '12',
  );
});
