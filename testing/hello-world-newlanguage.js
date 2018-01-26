const compileDiamond = require('./compile-diamond');
const blockchain = require('./blockchain');

async function main() {
  const client = blockchain.createBlockchainClient();
  const coinbase = await client.eth.getCoinbase();

  const simpleTokenCode = await compileDiamond.compile('./test_programs/unit.dia');
  const simpleTokenContract = new client.eth.Contract(simpleTokenCode.interface);

  const deployedContract = await simpleTokenContract
    .deploy({ data: simpleTokenCode.bytecode, arguments: [] })
    .send({ from: coinbase, gas: 300000000000000 });

  deployedContract.setProvider(client.currentProvider);
  console.log(await deployedContract.methods.main().call({ from: coinbase, gas: 30000 }));
}

main().catch(console.error);
