/* Characters */

CHAR_EOL => 10.
CHAR_SPACE => 32.
CHAR_LEFT_BRACKET => 40.
CHAR_RIGHT_BRACKET => 41.
CHAR_ASTERISK => 42.
CHAR_PLUS => 43.
CHAR_MINUS => 45.
CHAR_COLON => 58.
CHAR_LESS => 60.
CHAR_GREATER => 62.
CHAR_QUERY => 63.

/* Bounds */

MAXINT => 33554431.

/* From the Emulator */

FALSE => 0.
TRUE  => 1.

/* Action Codes for the Foreign Kernel */

PSI_POST =>            1.
PSI_CLOSE =>           2.
PSI_STEP =>            3.
PSI_INDEX =>           4.

/* Sub-Channel Indices */

PSI_BLOCKED	     =>  1.	% TRUE iff non-empty queue and.
				% no transmission is possible
PSI_CHANNEL_TYPE     =>  2.	% see below.
PSI_CHANNEL_RATE     =>  3.	% Real.
PSI_CHANNEL_REFS     =>  4.	% Reference counter.
PSI_SEND_ANCHOR      =>  5.	% Head of SendQueue.
PSI_DIMER_ANCHOR     =>  5.	% Head of DimerQueue.
PSI_SEND_WEIGHT      =>  6.	% Sum of Send Multipliers.
PSI_DIMER_WEIGHT     =>  6.	% Sum of Dimer Multipliers.
PSI_RECEIVE_ANCHOR   =>  7.	% Head of ReceiveQueue.
PSI_RECEIVE_WEIGHT   =>  8.	% Sum of Receive Multipliers.
PSI_WEIGHT_TUPLE     =>  9.	% (constant) Weight computation parameters
PSI_NEXT_CHANNEL     => 10.	% (circular) Channel list.
PSI_PREVIOUS_CHANNEL => 11.	% (circular) Channel list.
PSI_CHANNEL_NAME     => 12.	% (constant) Created Channel name.

CHANNEL_SIZE => 12.

/* Channel Types */

PSI_CHANNEL_ANCHOR =>  0.
PSI_UNKNOWN =>         1.
PSI_BIMOLECULAR =>     2.
PSI_HOMODIMERIZED =>   3.
PSI_INSTANTANEOUS =>   4.
PSI_SINK =>            5.

/* Weight Computation Values */

PSI_DEFAULT_WEIGHT_INDEX => 0.
PSI_DEFAULT_WEIGHT_NAME  => default.

/* Message Types */

PSI_MESSAGE_ANCHOR =>  0.
PSI_SEND =>            1.
PSI_RECEIVE =>         2.
PSI_DIMER =>           3.

/* Listed Operation tuple (1-5), Queued message tuple (1-8) */

PSI_MS_TYPE =>         1.		/* One of Message Types */
PSI_MS_CID =>          2.
PSI_MS_CHANNEL =>      3.
PSI_MS_MULTIPLIER =>   4.		/* Positive Integer */
PSI_MS_TAGS =>         5.
PSI_SEND_TAG =>        5.
PSI_RECEIVE_TAG =>     6.
PSI_COMMON =>          7.		/* {PId, MsList, Value^, Chosen^} */
PSI_MESSAGE_LINKS =>   8.		/* (circular) stored fcp 2-vector */

PSI_MESSAGE_SIZE =>    8.

PSI_NEXT_MS =>         1.
PSI_PREVIOUS_MS =>     2.

/* Operation request tuple (1-5), Transmission common tuple (1-4) */

PSI_OP_PID =>          1.
PSI_OP_MSLIST =>       2.
PSI_OP_VALUE =>        3.
PSI_OP_CHOSEN =>       4.
PSI_OP_REPLY =>        5.

PSI_COMMON_SIZE =>     4.
