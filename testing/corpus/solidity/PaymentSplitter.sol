pragma solidity ^0.4.8;

contract PaymentSplitter {
    address account1;
    address account2;
    uint account1Balance;
    uint account2Balance;

    function PaymentSplitter() public {
        account1 = msg.sender;
    }

    function set_account_2(address addr) public {
        require(msg.sender == account1);
        account2 = addr;
    }

    function pay() public payable {
        require(msg.value % 2 == 0);
        account1Balance += msg.value / 2;
        account2Balance += msg.value / 2;
    }

    function claim_balance() public returns (uint) {
        require(msg.sender == account1 || msg.sender == account2);
        uint toSend;

        if (msg.sender == account1) {
            toSend = account1Balance;
            account1Balance = 0;
        } else {
            toSend = account2Balance;
            account2Balance = 0;
        }

        msg.sender.transfer(toSend);
        return toSend;
    }
}
