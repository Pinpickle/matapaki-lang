const { test } = require('ava');
const { testDiamond } = require('../utils/blockchain');
const { runEvm } = require('../utils/evm');

const receiver_address = '0x2c085463e2d726cbb98e1222c89506e82d90890e';

test('Payable method', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/environment.dia' });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '0',
  );

  t.deepEqual(
    await contract.methods.pay_me().call({ from: coinbase, gas: 100000, value: 5 }),
    '5',
    'Method should return amount sent',
  );
  
  await contract.methods.pay_me().send({ from: coinbase, gas: 100000, value: 5 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '5',
  );
});

test('Other method not payable', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/environment.dia' });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '0',
  );

  await t.throws(
    contract.methods.sender().send({ from: coinbase, gas: 100000, value: 5 }),
    error => error.message === 'Can not send value to non-payable contract method or constructor',
  );

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '0',
  );
});

test('Sender works', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/environment.dia' });

  t.deepEqual(
    (await contract.methods.sender().call({ from: coinbase, gas: 100000 })).toLowerCase(),
    coinbase.toLowerCase(),
  );
});

test('Balance works', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/environment.dia' });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '0',
  );

  t.deepEqual(
    await contract.methods.my_balance().call({ from: coinbase, gas: 100000 }),
    '0',
  );

  await contract.methods.pay_me().send({ from: coinbase, gas: 100000, value: 5 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '5',
  );

  t.deepEqual(
    await contract.methods.my_balance().call({ from: coinbase, gas: 100000 }),
    '5',
  );
});

test('Address works', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/environment.dia' });

  t.deepEqual(
    (await contract.methods.my_address().call({ from: coinbase, gas: 100000 })).toLowerCase(),
    contract.options.address.toLowerCase(),
  );
});