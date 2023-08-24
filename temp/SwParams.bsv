`include "ConnectalProjectConfig.bsv"
import ShaleUtil::*;

typedef NUM_OF_SERVERS NUM_OF_SWITCH_PORTS;
typedef Bit#(9) PortIndex;

typedef 64 BITS_PER_CYCLE; //for 10Gbps interface and 156.25MHz clock freq

// typedef 2048 CELL_SIZE;
