const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');

test('Single bindings', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/let.dia' });

  t.deepEqual(
    await contract.methods.single_let_binding().call({ from: coinbase, gas: 40000 }),
    '6',
  );
});

test('Multiple bindings', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/let.dia' });

  t.deepEqual(
    await contract.methods.multiple_let_bindings().call({ from: coinbase, gas: 40000 }),
    '21',
  );
});
