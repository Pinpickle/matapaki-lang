const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');
const { runEvm } = require('../utils/evm');

const receiver_address = '0x2c085463e2d726cbb98e1222c89506e82d90890e';

test('Contract sends ether', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/send_ether.dia' }, { value: 2 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '2',
  );

  t.deepEqual(
    await client.eth.getBalance(receiver_address),
    '0',
  );

  await contract.methods.send_ether_to_address().send({ from: coinbase, gas: 100000 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '0',
  );

  t.deepEqual(
    await client.eth.getBalance(receiver_address),
    '2',
  );
});

test('UNCHECKED SEND: Failed send propagates', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/send_ether.dia' }, { value: 1 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    // Not enough to send 2
    '1',
  );

  t.deepEqual(
    await client.eth.getBalance(receiver_address),
    '0',
  );

  await t.throws(
    contract.methods.send_ether_to_address().send({ from: coinbase, gas: 100000 }),
    (error) => error.message === 'VM Exception while processing transaction: revert',
  );
});
