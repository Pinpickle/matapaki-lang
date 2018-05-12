const csvStringify = require('csv-stringify');
const fs = require('fs');

const { testCases, runAllTests, timeAllContracts, testAllFactorial } = require('./test_corpus');

function meanOfSamples(samples) {
  if (samples.length === 0) {
    return 0;
  }

  return samples.reduce((mean, sample) => mean + sample, 0) / samples.length;
}

function confidenceIntervalOfSamples(samples) {
  if (samples.length <= 1) {
    return 0;
  }

  const mean = meanOfSamples(samples);

  // 95% confidence interval
  return Math.sqrt(samples.reduce((error, sample) => (sample - mean) ** 2, 0));
}

function testDataToCSVRecords(testData) {
  const methods = [];
  const contractGases = [];
  const contractBinaries = [];

  for (const contractTest of testData) {
    contractTest.diamond.methods.forEach((method, index) => {
      methods.push([
        (contractTest.contract + ' ' + method.name).replace(/_/g, '\\_'),
        contractTest.solidity.methods[index].gasUsed,
        method.gasUsed,
        Math.log10(contractTest.solidity.methods[index].gasUsed),
        Math.log10(method.gasUsed),
      ]);
    });

    contractGases.push(
      [
        contractTest.contract.replace(/_/g, '\\_'),
        contractTest.solidity.contract.gasUsed,
        contractTest.diamond.contract.gasUsed,
        Math.log10(contractTest.solidity.contract.gasUsed),
        Math.log10(contractTest.diamond.contract.gasUsed),
      ],
    );

    contractBinaries.push(
      [
        contractTest.contract.replace(/_/g, '\\_'),
        contractTest.solidity.contract.binarySize,
        contractTest.diamond.contract.binarySize,
        Math.log10(contractTest.solidity.contract.binarySize),
        Math.log10(contractTest.diamond.contract.binarySize),
      ]
    );
  }

  return {
    methods: [
      ['name', 'solidity', 'diamond', 'logsolidity', 'logdiamond'],
      ...methods
        .sort(([, solidityAGas], [, solidityBGas]) => solidityAGas - solidityBGas),
    ],
    contractGases: [
      ['name', 'solidity', 'diamond', 'logsolidity', 'logdiamond'],
      ...contractGases
        .sort(([, solidityAGas], [, solidityBGas]) => solidityAGas - solidityBGas)
    ],
    contractBinaries: [
      ['name', 'solidity', 'diamond', 'logsolidity', 'logdiamond'],
      ...contractBinaries
        .sort(([, solidityABinary], [, solidityBBinary]) => solidityABinary - solidityBBinary)
    ],
  }
}

function factorialDataToCSVRecords(factorialData) {
  const rows = [];

  factorialData.solidity.forEach((result, index) => {
    rows.push([index, result, factorialData.diamond[index]]);
  })

  return [
    ['input', 'solidity', 'diamond'],
    ...rows
  ];
}

function timeDataToCSVRecords(timeData) {
  const times = [];

  for (const result of timeData) {
    times.push([
      result.contract.replace(/_/g, '\\_'),
      ...[
        meanOfSamples(result.solidity),
        confidenceIntervalOfSamples(result.solidity),
        meanOfSamples(result.diamond),
        confidenceIntervalOfSamples(result.diamond)].map(x => Math.round(x * 1000) / 1000),
      ],
    );
  }

  return [
    ['name', 'solidity', 'solidityerror', 'diamond', 'diamonderror'],
    ...times
      .sort(([, solidityAMean], [, solidityBMean]) => solidityAMean - solidityBMean),
  ];
}

function exportTestsCSV() {
  return runAllTests(testCases).then(testDataToCSVRecords).then(({ methods, contractGases, contractBinaries }) => {
    csvStringify(methods, (err, output) => {
      fs.writeFileSync('experiment/methods.csv', output);
    });

    csvStringify(contractBinaries, (err, output) => {
      fs.writeFileSync('experiment/contract_binary_sizes.csv', output);
    });

    csvStringify(contractGases, (err, output) => {
      fs.writeFileSync('experiment/contract_gas_consumption.csv', output);
    });
  });
}

function exportTimesCSV() {
  return timeAllContracts(testCases).then(timeDataToCSVRecords).then(times => {
    csvStringify(times, (err, output) => {
      fs.writeFileSync('experiment/compile_times.csv', output);
    });
  });
}

function exportFactorialCSV() {
  return testAllFactorial().then(factorialDataToCSVRecords).then(rows => {
    csvStringify(rows, (err, output) => {
      fs.writeFileSync('experiment/factorial_gas.csv', output);
    });
  });
}

exportTestsCSV();
exportTimesCSV();
//exportFactorialCSV();
