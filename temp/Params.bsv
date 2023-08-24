import ShaleUtil::*;

`include "ConnectalProjectConfig.bsv"

typedef 64 BITS_PER_CYCLE; //for 10Gbps interface and 156.25MHz clock freq

`ifdef MULTI_NIC
typedef NUM_OF_SERVERS NUM_OF_ALTERA_PORTS;
`else
typedef 1 NUM_OF_ALTERA_PORTS;
`endif

// The current time input given to PIEO while dequeuing
// will be a bitmap for all buckets to mark eligibility.
// The number of bits = num of token buckets.
`ifdef LIMIT_ACTIVE_BUCKETS
typedef Bit#(NUM_ACTIVE_BUCKETS) PIEOCurrentTime;
`else
typedef Bit#(NUM_FWD_TOKEN_BUCKETS) PIEOCurrentTime;
`endif

// Bits to store initial num of tokens per bucket, which is also the 
// max num of tokens per bucket for any node at any given time.
typedef 1 TOKEN_COUNT_SIZE;