const csvStringify = require('csv-stringify');
const fs = require('fs');

const { testCases, runAllTests, timeAllContracts } = require('./test_corpus');

function meanOfSamples(samples) {
  if (samples.length === 0) {
    return 0;
  }

  return samples.reduce((mean, sample) => mean + sample, 0) / samples.length;
}

function errorOfSamples(samples) {
  if (samples.length <= 1) {
    return 0;
  }

  const mean = meanOfSamples(samples);

  return Math.sqrt(samples.reduce((error, sample) => (sample - mean) ** 2, 0) / (samples.length - 1)) / Math.sqrt(samples.length);
}

function testDataToCSVRecords(testData) {
  const methods = [];
  const contracts = [];

  for (const contractTest of testData) {
    contractTest.diamond.methods.forEach((method, index) => {
      methods.push([
        contractTest.contract + ' ' + method.name,
        contractTest.solidity.methods[index].gasUsed,
        method.gasUsed,
      ]);
    });
  }

  return {
    methods: [
      ['name', 'solidity', 'diamond'],
      ...methods
        .sort(([nameA, solidityAGas], [nameB, solidityBGas]) => solidityAGas - solidityBGas),
    ],
    contracts: [],
  }
}

function timeDataToCSVRecords(timeData) {
  const times = [];

  for (const result of timeData) {
    times.push([meanOfSamples(result.solidity), errorOfSamples(result.solidity), meanOfSamples(result.diamond), errorOfSamples(result.diamond)].map(x => Math.round(x * 1000) / 1000));
  }

  return [
    ['x', 'solidity', 'solidityerror', 'diamond', 'diamonderror'],
    ...times
      .sort(([solidityAMean], [solidityBMean]) => solidityAMean - solidityBMean)
      .map((data, index) => [index, ...data]),
  ];
}

function exportTestsCSV() {
  return runAllTests(testCases).then(testDataToCSVRecords).then(({ methods }) => {
    csvStringify(methods, (err, output) => {
      fs.writeFileSync('experiment/methods.csv', output);
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

exportTestsCSV();

