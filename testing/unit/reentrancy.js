const { test } = require('ava');
const { testDiamond, testSolidity } = require('../utils/blockchain');
const { runEvm } = require('../utils/evm');
const { compile: compileSolidity } = require('../utils/compile-solidity');

async function createAttackerContract(client, coinbase) {
  const compiledContract = await compileSolidity({ pathName: __dirname + '/programs/reentrancy/Attacker.sol', contractName: 'Attacker' });

  const attackerContractBase = new client.eth.Contract(compiledContract.interface);
  const attackerContract = await attackerContractBase
    .deploy({ data: compiledContract.bytecode, arguments: [] })
    .send({ from: coinbase, gas: 10000000 });
  
  attackerContract.setProvider(client.currentProvider);

  return attackerContract;
}

test('Honey pot works', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/reentrancy/honey_pot.dia' }, { value: 10 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '10',
  );

  await contract.methods.claim_money().send({ from: coinbase, gas: 100000 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '9',
  );

  await contract.methods.claim_money().send({ from: coinbase, gas: 100000 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '9',
  );
});

test('Solidity honey pot works', async t => {
  const { contract, coinbase, client } = await testSolidity({ pathName: __dirname + '/programs/reentrancy/HoneyPot.sol', contractName: 'HoneyPot' }, { value: 10 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '10',
  );

  await contract.methods.claim_money().send({ from: coinbase, gas: 100000 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '9',
  );

  await contract.methods.claim_money().send({ from: coinbase, gas: 100000 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '9',
  );
})

test('Re-entrancy attack works', async t => {
  const { contract, coinbase, client } = await testSolidity({ pathName: __dirname + '/programs/reentrancy/HoneyPot.sol', contractName: 'HoneyPot' }, { value: 10 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '10',
  );

  const attackerContract = await createAttackerContract(client, coinbase);

  t.deepEqual(
    await client.eth.getBalance(attackerContract.options.address),
    '0',
  );

  await attackerContract.methods.start(contract.options.address).send({ from: coinbase, gas: 10000000 });

  t.deepEqual(
    await client.eth.getBalance(attackerContract.options.address),
    '3',
  );

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '7',
  );

  await contract.methods.claim_money().send({ from: coinbase, gas: 100000 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '7',
  );
});

test('Re-entrancy is prevented', async t => {
  const { contract, coinbase, client } = await testDiamond({ pathName: __dirname + '/programs/reentrancy/honey_pot.dia' }, { value: 10 });

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '10',
  );

  const attackerContract = await createAttackerContract(client, coinbase);
  
  t.deepEqual(
    await client.eth.getBalance(attackerContract.options.address),
    '0',
  );

  await t.throws(
    attackerContract.methods.start(contract.options.address).send({ from: coinbase, gas: 100000 }),
    error => error.message === 'VM Exception while processing transaction: revert',
  );

  t.deepEqual(
    await client.eth.getBalance(attackerContract.options.address),
    '0',
  );

  t.deepEqual(
    await client.eth.getBalance(contract.options.address),
    '10',
  );
});
