const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');

test('True works', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/require.dia' });

  t.deepEqual(
    await contract.methods.require_true(true).call({ from: coinbase, gas: 40000 }),
    true,
  );
});

test('False rejects', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/require.dia' });

  await t.throws(
    contract.methods.require_true(false).call({ from: coinbase, gas: 40000 }),
    error => error.message === 'VM Exception while processing transaction: revert',
  );
});