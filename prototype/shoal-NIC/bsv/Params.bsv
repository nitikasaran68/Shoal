`include "ConnectalProjectConfig.bsv"
import ShaleUtil::*;

typedef 64 BITS_PER_CYCLE; //for 10Gbps interface and 156.25MHz clock freq

`ifdef MULTI_NIC
typedef NUM_OF_SERVERS NUM_OF_ALTERA_PORTS;
`else
typedef 1 NUM_OF_ALTERA_PORTS;
`endif

