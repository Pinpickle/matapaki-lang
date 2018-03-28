
const fs = require('fs');
const util = require('util');
const readFile = util.promisify(fs.readFile);
const exec = require('./exec');

async function compile({ pathName }) {
  return JSON.parse((await exec(`./_build/default/compiler/src/main.exe ${pathName}`)).replace('\n', ''));
}

module.exports = {
  compile,
};
