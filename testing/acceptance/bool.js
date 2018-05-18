const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');

test('And', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/bool.dia' });

  t.deepEqual(
    await contract.methods.and().call({ from: coinbase, gas: 40000 }),
    false,
  );
});

test('Or', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/bool.dia' });

  t.deepEqual(
    await contract.methods.or().call({ from: coinbase, gas: 40000 }),
    true,
  );
});
