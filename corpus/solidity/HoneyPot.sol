pragma solidity ^0.4.8;

contract HoneyPot {
  mapping (address => uint) public balances;

  function HoneyPot() public payable {
    put();
  }

  function put() public payable {
    balances[msg.sender] = msg.value;
  }

  function get() public {
    balances[msg.sender] = 0;
    require(msg.sender.call.value(balances[msg.sender])());
  }

  function() public {
    revert();
  }
}
