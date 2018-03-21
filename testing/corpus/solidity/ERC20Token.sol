// Adapted from https://theethereum.wiki/w/index.php/ERC20_Token_Standard

pragma solidity ^0.4.8;

contract ERC20Token {
  // Balances for each account
  mapping(address => uint256) balances;

  // Owner of account approves the transfer of an amount to another account
  mapping(address => mapping (address => uint256)) allowed;

  function ERC20Token() public {
    balances[msg.sender] = totalSupply();
  }

  function totalSupply() public pure returns (uint) {
    return 100000;
  }

  // Get the token balance for account `tokenOwner`
  function balanceOf(address tokenOwner) public constant returns (uint balance) {
    return balances[tokenOwner];
  }

  function allowance(address tokenOwner, address spender) public constant returns (uint remaining) {
    return allowed[tokenOwner][spender];
  }

  // Transfer the balance from owner's account to another account
  function transfer(address to, uint tokens) public returns (bool success) {
    require(balances[msg.sender] >= tokens);
    balances[msg.sender] = balances[msg.sender] - tokens;
    balances[to] = balances[to] + tokens;
    return true;
  }

  // Allow `spender` to withdraw from your account, multiple times, up to the `tokens` amount.
  // If this function is called again it overwrites the current allowance with _value.
  function approve(address spender, uint tokens) public returns (bool success) {
    allowed[msg.sender][spender] = tokens;
    return true;
  }

  // Send `tokens` amount of tokens from address `from` to address `to`
  // The transferFrom method is used for a withdraw workflow, allowing contracts to send
  // tokens on your behalf, for example to "deposit" to a contract address and/or to charge
  // fees in sub-currencies; the command should fail unless the _from account has
  // deliberately authorized the sender of the message via some mechanism; we propose
  // these standardized APIs for approval:
  function transferFrom(address from, address to, uint tokens) public returns (bool success) {
    require(balances[from] >= tokens);
    require(allowed[from][msg.sender] >= tokens);
    balances[from] = balances[from] - tokens;
    allowed[from][msg.sender] = allowed[from][msg.sender] - tokens;
    balances[to] = balances[to] + tokens;
    return true;
  }
}


 
    
 
    
 
    
 
    