const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');
const { runEvm } = require('../utils/evm');

test('Set values work', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/simple_mapping.dia' });

  t.deepEqual(
    await contract.methods.value_of(0).call({ from: coinbase, gas: 40000 }),
    '1',
  );

  t.deepEqual(
    await contract.methods.value_of(1).call({ from: coinbase, gas: 40000 }),
    '2',
  );
});

test('Default values work', async t => {
  const { contract, coinbase } = await testDiamond({ pathName: __dirname + '/programs/simple_mapping.dia' });

  t.deepEqual(
    await contract.methods.value_of(1024).call({ from: coinbase, gas: 40000 }),
    '0',
  );
});

test('Default values for records work', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/simple_mapping.dia' });

  t.deepEqual(
    await contract.methods.default_record().call({ from: coinbase, gas: 40000 }),
    '0',
  );
});

test('Mappings of mappings', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/simple_mapping.dia' });

  t.deepEqual(
    await contract.methods.value_of_mapping_of_mapping(10, 20).call({ from: coinbase, gas: 40000 }),
    '30',
  );

  t.deepEqual(
    await contract.methods.value_of_mapping_of_mapping(10, 10).call({ from: coinbase, gas: 40000 }),
    '0',
  );

  t.deepEqual(
    await contract.methods.value_of_mapping_of_mapping(20, 20).call({ from: coinbase, gas: 40000 }),
    '0',
  );
});
