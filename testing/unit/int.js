const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');

// Currently broken
/*
test('Operator presedence', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/int.dia' });

  t.deepEqual(
    await contract.methods.operator_presedence().call({ from: coinbase, gas: 40000 }),
    '981',
  );
});
*/

test('Addition', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/int.dia' });

  t.deepEqual(
    await contract.methods.addition().call({ from: coinbase, gas: 40000 }),
    '15',
  );
});

test('Subtraction', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/int.dia' });

  t.deepEqual(
    await contract.methods.subtraction().call({ from: coinbase, gas: 40000 }),
    '5',
  );
});

test('Complicated', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/int.dia' });

  t.deepEqual(
    await contract.methods.complicated().call({ from: coinbase, gas: 40000 }),
    '55',
  );
});