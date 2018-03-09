const { compile: compileDiamond } = require('../utils/compile-diamond.js');
const { compile: compileSolidity } = require('../utils/compile-solidity.js');
const { createBlockchainClient } = require('../utils/blockchain');

const testCases = [
  require('./basic_arith'),
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
  return process.hrtime(start);
}

async function timeFunctionRepeats(toTime, repeats = 5) {
  const results = [];

  for (let attempt = 0; attempt < repeats; attempt += 1) {
    results.push(await timeFunction(toTime));
  }

  return results;
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
          .send({ from: coinbase, gas: 10000000, value: 0 });
        
        deployedContract.setProvider(client.currentProvider);

        const method = await methodTest.create({ contract: deployedContract, coinbase, client });
        
        const resultStats = await method.send({ from: coinbase, gas: 10000000 });
        const result = await method.call({ from: coinbase, gas: 10000000 });

        if ((methodTest.test) && (!(await methodTest.test({ result, client, coinbase, contract })))) {
          throw new Error('Method failed!', methodTest.name);
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
      diamond: await runTest(diamondCompiled, testCase),
      solidity: await runTest(solidityCompiled, testCase),
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

timeAllContracts(testCases).then(results => {
  console.log(JSON.stringify(results, null, '  '));
}).catch(console.error);
