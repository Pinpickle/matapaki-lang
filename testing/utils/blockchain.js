const Web3 = require('web3');
const ganache = require('ganache-cli');
const fs = require('fs');
const util = require('util');
const compileSolidity = require('./compile-solidity');
const compileDiamond = require('./compile-diamond');

function createBlockchainClient() {
  return new Web3(ganache.provider({
    gasLimit: 900000000000000,
    gasPrice: 1,
    debug: true,
    accounts: [
      {
        balance: 1000000000000000000000000,
      },
    ],
  }));
}

function createTester(compiler) {
  return async (compilerArgs, { deployArguments = [], value = '0' } = { }) => {
    const client = createBlockchainClient();
    const coinbase = await client.eth.getCoinbase();

    const { bytecode, interface } = await compiler(compilerArgs);

    const contract = new client.eth.Contract(interface);
    const deployedContract = await contract
      .deploy({ data: bytecode, arguments: deployArguments })
      .send({ from: coinbase, gas: 10000000, value });
    
    deployedContract.setProvider(client.currentProvider);

    return {
      contract: deployedContract,
      client,
      coinbase,
    };
  }
}

/*await (new Promise ((resolve) => { client.currentProvider.sendAsync({
      method: "debug_traceTransaction",
      params: [tx.transactionHash, {}],
      jsonrpc: "2.0",
      id: "2"
  }, (err, logs) => (fs.writeFileSync('./experiment/result', JSON.stringify(logs, null, 2)), resolve())) }));*/
const testSolidity = createTester(compileSolidity.compile);
const testDiamond = createTester(compileDiamond.compile);

async function inspectTransaction(client, transaction, path = 'result') {
  const result = await (new Promise ((resolve, reject) => {
    client.currentProvider.sendAsync({
      method: "debug_traceTransaction",
      params: [transaction.transactionHash, {}],
      jsonrpc: "2.0",
      id: "2"
    }, (err, logs) => {
      if (err) {
        reject(err);
      } else {
        resolve(logs);
      }
    });
  }));

  await util.promisify(fs.writeFile)('./experiment/' + path, JSON.stringify(result, null, 2));
}

module.exports = {
  createBlockchainClient,
  testSolidity,
  testDiamond,
  inspectTransaction,
};

