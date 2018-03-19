pragma solidity ^0.4.8;

contract Factorial {
  function factorial(int n) pure public returns (int) {
    require (n >= 0);

    if (n == 0) {
      return 1;
    }

    int result = 1;
    for (int step = n; step >= 1; step -= 1) {
      result *= step;
    }

    return result;
  }
}
