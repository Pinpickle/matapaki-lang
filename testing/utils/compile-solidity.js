const fs = require('fs');
const util = require('util');
const solc = require('solc');
const exec = require('./exec');
const readFile = util.promisify(fs.readFile);

/**
 * Matches the solc API but uses the native binary for faster, more accurate compiles
 * 
 * Adapted from https://medium.com/@viktornosov/truffle-compile-so-slow-solc-is-much-faster-2acfb7004bbe
 */
async function compileSolidityUsingExecutable(path) {
  const stdout = await exec(`solc --optimize --abi --bin ${path}`, { maxBuffer: 1024 * 10000 });
  const compiled = {};

  const parts = stdout.split('=======');

  for (let pos = 1; pos < parts.length; pos += 2) {
    let [contractPath, contractName] = parts[pos].trim().split(':');

    let [, , bytecode, , interface] = parts[pos + 1].split("\n");
    compiled[':' + contractName] = {
      interface,
      bytecode,
    };
  }

  return compiled;
}

async function compile({ pathName, contractName }) {
  // Compile contracts are stored in an object with keys ":ContractName"
  // So we need to extract that first
  const { [`:${contractName}`]: compiledContract } = await compileSolidityUsingExecutable(pathName);;

  return {
    interface: JSON.parse(compiledContract.interface),
    bytecode: compiledContract.bytecode,
  };
}

module.exports = {
  compile,
};
