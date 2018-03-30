pragma solidity ^0.4.8;

contract OpenAuction {
    address private beneficiary;
    uint private highestBid;
    address private highestBidder;
    mapping(address => uint) private pendingReturns;
    bool private auctionRunning;

    function OpenAuction() public {
        beneficiary = msg.sender;
        auctionRunning = true;
    }

    function bid() payable public {
        require(auctionRunning);
        require(msg.value > highestBid);

        pendingReturns[highestBidder] += highestBid;

        highestBidder = msg.sender;
        highestBid = msg.value;
    }

    function withdraw() public {
        uint amount = pendingReturns[msg.sender];

        if (amount > 0) {
            pendingReturns[msg.sender] = 0;
            msg.sender.transfer(amount);
        }
    }

    function highest_bidder() public view returns (address) {
        return highestBidder;
    }

    function highest_bid() public view returns (uint) {
        return highestBid;
    }

    function end_auction() public {
        require(msg.sender == beneficiary);
        require(auctionRunning);
        auctionRunning = false;
        beneficiary.transfer(highestBid);
    }
}
