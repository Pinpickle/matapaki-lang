const Web3 = require('web3');
const ganache = require('ganache-cli');
const compileSolidity = require('./compile-solidity');
const compileDiamond = require('./compile-diamond');

function createBlockchainClient() {
  return new Web3(ganache.provider({
    gasLimit: 900000000000000,
    gasPrice: 1,
    accounts: [
      {
        balance: 1000000000000000000000000,
      },
    ],
  }));
}

function createTester(compiler) {
  return async (compilerArgs, { deployArguments = [] } = { }) => {
    const client = createBlockchainClient();
    const coinbase = await client.eth.getCoinbase();

    const { bytecode, interface } = await compiler(compilerArgs);

    const contract = new client.eth.Contract(interface);
    const deployedContract = await contract
      .deploy({ data: bytecode, arguments: deployArguments })
      .send({ from: coinbase, gas: 30000000 });
    
    deployedContract.setProvider(client.currentProvider);

    return {
      contract: deployedContract,
      client,
      coinbase,
    };
  }
}

const testSolidity = createTester(compileSolidity.compile);
const testDiamond = createTester(compileDiamond.compile);

module.exports = {
  createBlockchainClient,
  testSolidity,
  testDiamond,
};

