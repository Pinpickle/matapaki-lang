const fs = require('fs');
const util = require('util');
const solc = require('solc');

const readFile = util.promisify(fs.readFile);

async function compile({ pathName, contractName }) {
  const file = await readFile(pathName, 'utf8');

  // Compile contracts are stored in an object with keys ":ContractName"
  // So we need to extract that first
  const { [`:${contractName}`]: compiledContract } = solc.compile(file, 0).contracts;

  return {
    interface: JSON.parse(compiledContract.interface),
    bytecode: compiledContract.bytecode,
  };
}

module.exports = {
  compile,
};
