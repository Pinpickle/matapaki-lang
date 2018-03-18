// Adapted from https://ethereum.org/token

pragma solidity ^0.4.8;

contract SimpleToken {
    /* This creates an array with all balances */
    mapping (address => int256) public balanceOf;

    /* Initializes contract with initial supply tokens to the creator of the contract */
    function SimpleToken() {
        balanceOf[msg.sender] = 1000000;              // Give the creator all initial tokens
    }

    /* Send coins */
    function transfer(address _to, int256 _value) public {
        require(balanceOf[msg.sender] >= _value);           // Check if the sender has enough
        require(balanceOf[_to] + _value >= balanceOf[_to]); // Check for overflows
        balanceOf[msg.sender] -= _value;                    // Subtract from the sender
        balanceOf[_to] += _value;                           // Add the same to the recipient
    }

    function balance(address holder) public returns (int256) {
      return balanceOf[holder];
    }
}
