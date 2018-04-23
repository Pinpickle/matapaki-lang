pragma solidity ^0.4.8;

contract BinaryVote {
    uint yesVotes = 0;
    uint noVotes = 0;
    mapping(address => bool) votesPerformed;
    bool voteRunning;
    address runner;

    function BinaryVote() public {
        runner = msg.sender;
        voteRunning = true;
    }

    function vote(bool voteValue) public {
        require (!votesPerformed[msg.sender]);

        if (voteValue) {
            yesVotes += 1;
        } else {
            noVotes += 1;
        }

        votesPerformed[msg.sender] = true;
    }

    function end_vote() public {
        require (msg.sender == runner && voteRunning);
        voteRunning = false;
    }

    function get_winner() public returns (bool) {
        require (!voteRunning);
        return yesVotes >= noVotes;
    }
}
