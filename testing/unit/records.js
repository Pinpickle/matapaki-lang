const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');
const { runEvm } = require('../utils/evm');

test('Inputs', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/records.dia' });

  t.deepEqual(
    await contract.methods.add_two_numbers(7, 6).call({ from: coinbase, gas: 40000 }),
    '13',
  );
});

test('Outputs', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/records.dia' });

  const result = await contract.methods.output_twice(7).call({ from: coinbase, gas: 40000 });

  t.deepEqual(result.x, '7');
  t.deepEqual(result.y, '8');
});

test('Identity function on records', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/records.dia' });

  const result = await contract.methods.return_four_numbers(1, 2, 3, 4).call({ from: coinbase, gas: 40000 });

  t.deepEqual(result.x, '1');
  t.deepEqual(result.y, '2');
  t.deepEqual(result.z, '3');
  t.deepEqual(result.w, '4');
})

test('Nested records', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/records.dia' });

  t.deepEqual(
    await contract.methods.add_four_numbers_with_inner_records().call({ from: coinbase, gas: 40000 }),
    '7',
  );
});

test('Complex interplay between function calls and inner records', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/records.dia' });

  t.deepEqual(
    await contract.methods.add_four_numbers_complex(1, 2, 3, 4).call({ from: coinbase, gas: 40000 }),
    '10',
  );
});
