
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

async function compile({ pathName }) {
  return JSON.parse((await execPromised(`./_build/default/compiler/src/main.exe ${pathName}`)).replace('\n', ''));
}

module.exports = {
  compile,
};
