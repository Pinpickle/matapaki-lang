pragma solidity ^0.4.0;

contract HoneyPot {
    bool moneyClaimed = false;

    function HoneyPot() public payable {

    }

    function claim_money() public {
      if (!moneyClaimed) {
        msg.sender.call.value(1)();
        moneyClaimed = true;
      }
    }
}
