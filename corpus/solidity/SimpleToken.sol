// Adapted from https://ethereumbuilders.gitbooks.io/guide/content/en/solidity_tutorials.html

contract SimpleToken {
  address minter;
  mapping (address => uint) balances;

  function SimpleToken() public {
    minter = msg.sender;
  }

  function mint(address owner, uint amount) public {
    if (msg.sender != minter) {
      return;
    }

    balances[owner] += amount;
  }

  function send(address receiver, uint amount) public {
    if (balances[msg.sender] < amount) {
      return;
    }
    balances
      .set(msg.sender, balances.get(msg.sender) - amount)
      .set(receiver, balances.get(receiver) + amount);
  }

  function queryBalance(address addr) public constant returns (uint balance) {
    return balances[addr];
  }
}
