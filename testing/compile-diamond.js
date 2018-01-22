
const fs = require('fs');
const util = require('util');
const readFile = util.promisify(fs.readFile);
const { exec } = require('child_process');

function execPromised(...args) {
  return new Promise((resolve, reject) => {
    exec(...args, (error, stdout, stderr) => {
      if (error) {
        reject(error);
        return;
      }

      resolve(stdout);
    });
  });
}

async function compile(pathname) {
  const bytecode = (await execPromised(`./_build/default/compiler/src/main.exe ${pathname}`)).replace('\n', '');

  return {
    interface: [{
      inputs: [],
      payable: false,
      stateMutability: 'nonpayable',
      type: 'constructor'
    }, {
      inputs: [],
      outputs: [
        {
          type: "uint256",
          name: "blah"
        },
      ],
      payable: false,
      stateMutability: 'nonpayable',
      type: 'function',
      name: 'default',
      signature: '0xadsda',
    }],
    bytecode,
  };
}

module.exports = {
  compile,
};
