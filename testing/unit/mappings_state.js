const { test } = require('ava');
const util = require('util');
const { testDiamond, inspectTransaction } = require('../utils/blockchain');

test('Default values work', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/mappings_state.dia' });

  t.deepEqual(
    await contract.methods.get_from_one(1000).call({ from: coinbase, gas: 40000 }),
    '0',
  );
});

test('Updating values work', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/mappings_state.dia' });

  t.deepEqual(
    await contract.methods.get_from_one(1000).call({ from: coinbase, gas: 40000 }),
    '0',
  );

  await inspectTransaction(client, await contract.methods.set_to_one(1000, 50).send({ from: coinbase, gas: 400000 }));

  t.deepEqual(
    await contract.methods.get_from_one(1000).call({ from: coinbase, gas: 40000 }),
    '50',
  );
});

test('Swapping mappings fails', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/mappings_state.dia' });

  await t.throws(
    contract.methods.swap_one_and_two().send({ from: coinbase, gas: 40000 }),
    error => error.message === 'VM Exception while processing transaction: revert',
  );
});

test('Setting mappings of mappings', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/mappings_of_mappings_state.dia' });

  t.deepEqual(
    await contract.methods.get(1000, 2000).call({ from: coinbase, gas: 40000 }),
    '0',
  );

  await contract.methods.set(1000, 2000, 50).send({ from: coinbase, gas: 400000 });

  t.deepEqual(
    await contract.methods.get(1000, 2000).call({ from: coinbase, gas: 40000 }),
    '50',
  );
})