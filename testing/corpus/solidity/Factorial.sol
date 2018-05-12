pragma solidity ^0.4.8;

contract Factorial {
  function factorial(uint n) pure public returns (uint) {
    if (n == 0) {
      return 1;
    }

    uint result = 1;
    for (uint step = n; step >= 1; step -= 1) {
      result *= step;
    }

    return result;
  }
}
