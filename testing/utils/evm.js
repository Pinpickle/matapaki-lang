const { spawnSync } = require('child_process');

function runEvm(code, method) {
  spawnSync('evm', ['--code', code.replace('0x', ''), '--input', method.encodeABI().replace('0x', ''), '--debug', 'run'], { stdio: 'inherit' });
}

module.exports = {
  runEvm,
};
