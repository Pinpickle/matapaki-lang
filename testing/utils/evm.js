const { spawnSync } = require('child_process');

function runEvm(code, method) {
  spawnSync('evm', ['--code', code.replace('0x', ''), '--gas', '100000', '--input', method.encodeABI().replace('0x', ''), '--debug', 'run'], { stdio: 'inherit' });
}

module.exports = {
  runEvm,
};
