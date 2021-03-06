const { compile: compileDiamond } = require('../utils/compile-diamond.js');
const { compile: compileSolidity } = require('../utils/compile-solidity.js');
const { createBlockchainClient } = require('../utils/blockchain');

const testCases = [
  require('./tests/basic_arith'),
  require('./tests/simple_token'),
  require('./tests/factorial'),
  require('./tests/erc20_token'),
  require('./tests/open_auction'),
  require('./tests/binary_vote'),
  require('./tests/payment_splitter'),
];

async function getContractDeploymentMeasures({ interface, bytecode }) {
  const client = createBlockchainClient();
  const coinbase = await client.eth.getCoinbase();

  const contract = new client.eth.Contract(interface);
  let initReceipt;
  const deployedContract = await contract
    .deploy({ data: bytecode, arguments: [] })
    .send({ from: coinbase, gas: 10000000, value: 0 })
    .once('receipt', receipt => { initReceipt = receipt; });
  
  return {
    gasUsed: initReceipt.gasUsed,
    binarySize: bytecode.length / 2,
  };
}

async function timeFunction(toTime) {
  const start = process.hrtime();
  await toTime();
  const [seconds, nanoseconds] = process.hrtime(start);

  return seconds * 1000 + nanoseconds / 1000000
}

async function timeFunctionRepeats(toTime, repeats = 20) {
  const results = [];

  for (let attempt = 0; attempt < repeats; attempt += 1) {
    results.push(await timeFunction(toTime));
  }

  return results;
}

async function testFactorial({ interface, bytecode }) {
  const client = createBlockchainClient();
  const coinbase = await client.eth.getCoinbase();
  const contract = new client.eth.Contract(interface);
  const deployedContract = await contract
    .deploy({ data: bytecode, arguments: [] })
    .send({ from: coinbase, gas: 10000000, value: 0 })
    .catch(e => {
      console.log('Deploying failed');
      throw e;
    });
    
  deployedContract.setProvider(client.currentProvider);
  return await Promise.all((new Array(58)).fill(0).map(async (_, index) => (await deployedContract.methods.factorial(index).send({ from: coinbase, gas: 1000000 })).gasUsed));
}

async function testAllFactorial() {
  const compiledDiamond = await compileDiamond({ pathName: __dirname + '/diamond/factorial.dia' });
  const compiledSolidity = await compileSolidity({ pathName: __dirname + '/solidity/Factorial.sol', contractName: 'Factorial' });

  return {
    diamond: await testFactorial(compiledDiamond),
    solidity: await testFactorial(compiledSolidity),
  }
}

async function runTest({ interface, bytecode }, testCase) {
  return {
    methods:
      await Promise.all(testCase.methodTests.map(async methodTest => {
        const client = createBlockchainClient();
        const coinbase = await client.eth.getCoinbase();

        const contract = new client.eth.Contract(interface);
        const deployedContract = await contract
          .deploy({ data: bytecode, arguments: [] })
          .send({ from: coinbase, gas: 10000000, value: 0 })
          .catch(e => {
            console.log('Deploying failed');
            throw e;
          });
        
        deployedContract.setProvider(client.currentProvider);

        const method = await methodTest.create({ contract: deployedContract, coinbase, client });
        const value = methodTest.value || 0;
        const result = await method.call({ from: coinbase, gas: 10000000, value }).catch(error => {
          throw new Error(`Calling ${methodTest.name} failed! [${error.message}]`);
        });
        const resultStats = await method.send({ from: coinbase, gas: 10000000, value });

        if ((methodTest.test) && (!(await methodTest.test({ result, client, coinbase, contract: deployedContract })))) {
          throw new Error(`Testing ${methodTest.name} failed!`, methodTest.name);
        };

        return {
          name: methodTest.name,
          gasUsed: resultStats.gasUsed,
          result,
        };
      })),
    contract: await getContractDeploymentMeasures({ interface, bytecode }),
  };
}

async function runAllTests(testCases) {
  return Promise.all(testCases.map(async testCase => {
    const [diamondCompiled, solidityCompiled] = await Promise.all([
      compileDiamond({ pathName: __dirname + '/diamond/' + testCase.contract[0] + '.dia' }),
      compileSolidity({ pathName: __dirname + '/solidity/' + testCase.contract[1] + '.sol', contractName: testCase.contract[1] }),
    ]);

    return {
      contract: testCase.contract[0],
      diamond: await runTest(diamondCompiled, testCase).catch(e => {
        console.error('Problem with Diamond');
        throw e;
      }),
      solidity: await runTest(solidityCompiled, testCase).catch(e => {
        console.error('Problem with Solidity');
        throw e;
      }),
    };
  }));
}

async function timeAllContracts(testCases) {
  const results = [];

  // These should be run serially
  for (const testCase of testCases) {
    const diamondResult = await timeFunctionRepeats(() => compileDiamond({ pathName: __dirname + '/diamond/' + testCase.contract[0] + '.dia' }));
    const solidityResult = await timeFunctionRepeats(() => compileSolidity({ pathName: __dirname + '/solidity/' + testCase.contract[1] + '.sol', contractName: testCase.contract[1] }));

    results.push({
      contract: testCase.contract[0],
      diamond: diamondResult,
      solidity: solidityResult,
    });
  }

  return results;
}

module.exports = {
  runAllTests,
  timeAllContracts,
  testAllFactorial,
  testCases,
};
