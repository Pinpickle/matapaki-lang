const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');

test('Addition', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/uint.dia' });

  t.deepEqual(
    await contract.methods.addition().call({ from: coinbase, gas: 40000 }),
    '15',
  );
});

test('Subtraction', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/uint.dia' });

  t.deepEqual(
    await contract.methods.subtraction().call({ from: coinbase, gas: 40000 }),
    '5',
  );
});

test('Multiplication', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/uint.dia' });

  t.deepEqual(
    await contract.methods.multiplication().call({ from: coinbase, gas: 40000 }),
    '330',
  );
});

test('Division', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/uint.dia' });

  t.deepEqual(
    await contract.methods.division().call({ from: coinbase, gas: 40000 }),
    '3',
  );
});

test('Division with remainder', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/uint.dia' });

  t.deepEqual(
    await contract.methods.division_remainder().call({ from: coinbase, gas: 40000 }),
    '3',
  );
});

test('Modulo', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/uint.dia' });

  t.deepEqual(
    await contract.methods.mod().call({ from: coinbase, gas: 40000 }),
    '3',
  );
});

test('Complicated', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/uint.dia' });

  t.deepEqual(
    await contract.methods.complicated().call({ from: coinbase, gas: 40000 }),
    '60',
  );
});

test('Operator presedence', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/uint.dia' });

  t.deepEqual(
    await contract.methods.operator_presedence().call({ from: coinbase, gas: 40000 }),
    '939',
  );
});