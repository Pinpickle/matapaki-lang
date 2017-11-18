const Web3 = require('web3');
const TestRPC = require('ethereumjs-testrpc');
const fs = require('fs');
const util = require('util');

const readFile = util.promisify(fs.readFile);

const solc = require('solc');

async function compile(pathname) {
  const file = await readFile(pathname, 'utf8');

  return solc.compile(file, 0).contracts;
}

async function main() {
  const web3 = new Web3(TestRPC.provider({
    accounts: [
      {
        balance: 10000000000000000000000000000,
      },
    ],
  }));

  const coinbase = await web3.eth.getCoinbase();

  const { ':SimpleToken': simpleTokenCode } = await compile('./corpus/solidity/SimpleToken.sol');

  const simpleTokenContract = new web3.eth.Contract(JSON.parse(simpleTokenCode.interface));
  const deployedContract = await simpleTokenContract
    .deploy({ data: simpleTokenCode.bytecode, arguments: [] })
    .send({ from: coinbase, gas: 300000});

  console.log(await deployedContract.methods.queryBalance(coinbase).call({ from: coinbase, gas: 300000 }));
  console.log(await deployedContract.methods.mint(coinbase, 1000).send({ from: coinbase, gas: 300000 }));
  console.log(await deployedContract.methods.queryBalance(coinbase).call({ from: coinbase, gas: 300000 }));
  console.log(await web3.eth.getBalance(await web3.eth.getCoinbase()));
}

main().then(console.log, console.error);