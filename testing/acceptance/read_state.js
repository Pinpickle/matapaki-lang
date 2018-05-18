const { test } = require('ava');
const { testDiamond, inspectTransaction } = require('../utils/blockchain');
const { runEvm } = require('../utils/evm');

test('State is recalled', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/read_state.dia' });

  await inspectTransaction(client, await contract.methods.get_x().send({ from: coinbase, gas: 40000 }));

  t.deepEqual(
    await contract.methods.get_x().call({ from: coinbase, gas: 40000 }),
    '5',
  );

  t.deepEqual(
    await contract.methods.get_y_plus_arg(5).call({ from: coinbase, gas: 40000 }),
    '15',
  );

  t.deepEqual(
    await contract.methods.x_and_y_combination(1, 2).call({ from: coinbase, gas: 40000 }),
    '12',
  );
});
