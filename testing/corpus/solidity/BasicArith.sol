pragma solidity ^0.4.8;

contract BasicArith {
  function add(int x, int y) pure public returns (int) {
    return x + y;
  }

  function subtract(int x, int y) pure public returns (int) {
    return x - y;
  }
}
