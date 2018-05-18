const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');
const { runEvm } = require('../utils/evm');

test('Updates work', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/record_update.dia' });

  const result = await contract.methods.update_record().call({ from: coinbase, gas: 40000 });

  t.deepEqual(
    result.x,
    '5',
  );

  t.deepEqual(
    result.y,
    '10',
  );
});
