const compileSolidity = require('./compile-solidity');
const blockchain = require('./blockchain');

async function main() {
  const client = blockchain.createBlockchainClient();
  const coinbase = await client.eth.getCoinbase();
  
  const simpleTokenCode = await compileSolidity.compile('./corpus/solidity/SimpleToken.sol', 'SimpleToken');
  const simpleTokenContract = new client.eth.Contract(simpleTokenCode.interface);
  const deployedContract = await simpleTokenContract
    .deploy({ data: simpleTokenCode.bytecode, arguments: [] })
    .send({ from: coinbase, gas: 3000000 });
  
  deployedContract.setProvider(client.currentProvider);
  console.log(await client.eth.getCode(deployedContract.options.address));
  console.log(await deployedContract.methods.queryBalance(coinbase).call({ from: coinbase, gas: 300000 }));
  console.log(await deployedContract.methods.mint(coinbase, 1000).send({ from: coinbase, gas: 300000 }));
  console.log(await deployedContract.methods.queryBalance(coinbase).call({ from: coinbase, gas: 300000 }));
  console.log(await client.eth.getBalance(coinbase));
}

main().then(console.log, console.error);