const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');
const { runEvm } = require('../utils/evm');

test('State is modified', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/modify_state.dia' });

  t.deepEqual(
    await contract.methods.get_x().call({ from: coinbase, gas: 100000 }),
    '5',
  );

  await contract.methods.inc_x().send({ from: coinbase, gas: 100000 });

  t.deepEqual(
    await contract.methods.get_x().call({ from: coinbase, gas: 100000 }),
    '6',
  );
});

test('State is modified with record updates', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/modify_state_record_update.dia' });

  t.deepEqual(
    await contract.methods.get_x().call({ from: coinbase, gas: 100000 }),
    '5',
  );

  await contract.methods.inc_x().send({ from: coinbase, gas: 100000 });

  t.deepEqual(
    await contract.methods.get_x().call({ from: coinbase, gas: 100000 }),
    '6',
  );
})