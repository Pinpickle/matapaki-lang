const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');

test('Bools equal', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/compare.dia' });

  t.deepEqual(
    await contract.methods.bools_equal(true, false).call({ from: coinbase, gas: 40000 }),
    false,
  );

  t.deepEqual(
    await contract.methods.bools_equal(true, true).call({ from: coinbase, gas: 40000 }),
    true,
  );
});

test('Ints equal', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/compare.dia' });

  t.deepEqual(
    await contract.methods.ints_equal(10, 5).call({ from: coinbase, gas: 40000 }),
    false,
  );

  t.deepEqual(
    await contract.methods.ints_equal(10, 10).call({ from: coinbase, gas: 40000 }),
    true,
  );
});

test('Addresses equal', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/compare.dia' });

  t.deepEqual(
    await contract.methods.addresses_equal('0xe8a5eab3ade78f3a0fc21a193aab4b637310b97f', '0xbc398c2f3f71f99830c4f6384a8324a775290e7c').call({ from: coinbase, gas: 40000 }),
    false,
  );

  t.deepEqual(
    await contract.methods.addresses_equal('0xe8a5eab3ade78f3a0fc21a193aab4b637310b97f', '0xe8a5eab3ade78f3a0fc21a193aab4b637310b97f').call({ from: coinbase, gas: 40000 }),
    true,
  );
});

test('Greater', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/compare.dia' });

  t.deepEqual(
    await contract.methods.greater(10, 5).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.greater(10, -5).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.greater(10, 10).call({ from: coinbase, gas: 40000 }),
    false,
  );

  t.deepEqual(
    await contract.methods.greater(10, 15).call({ from: coinbase, gas: 40000 }),
    false,
  );
});

test('Greater or equal', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/compare.dia' });

  t.deepEqual(
    await contract.methods.greater_equal(10, 5).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.greater_equal(10, -5).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.greater_equal(10, 10).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.greater_equal(10, 15).call({ from: coinbase, gas: 40000 }),
    false,
  );
});

test('Lesser', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/compare.dia' });

  t.deepEqual(
    await contract.methods.lesser(5, 10).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.lesser(-5, 10).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.lesser(10, 10).call({ from: coinbase, gas: 40000 }),
    false,
  );

  t.deepEqual(
    await contract.methods.lesser(15, 10).call({ from: coinbase, gas: 40000 }),
    false,
  );
});

test('Greater or equal', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/compare.dia' });

  t.deepEqual(
    await contract.methods.lesser_equal(5, 10).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.lesser_equal(-5, 10).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.lesser_equal(10, 10).call({ from: coinbase, gas: 40000 }),
    true,
  );

  t.deepEqual(
    await contract.methods.lesser_equal(15, 10).call({ from: coinbase, gas: 40000 }),
    false,
  );
});

test('Operator precedence', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/compare.dia' });

  t.deepEqual(
    await contract.methods.operator_precedence().call({ from: coinbase, gas: 40000 }),
    true,
  );
});
