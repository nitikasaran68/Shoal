`include "ConnectalProjectConfig.bsv"

typedef 64 BITS_PER_CYCLE; //for 10Gbps interface and 156.25MHz clock freq
typedef BITS_PER_CYCLE HEADER_SIZE; //size of cell header

typedef 8 NUM_OF_SERVERS;       // N
typedef 3 NUM_OF_PHASES;        // h
typedef 2 NODES_PER_PHASE;       // (N ** 1/h)
typedef 1 PHASE_SIZE;           // NODES_PER_PHASE - 1
typedef Bit#(1) Coordinate      // ceil log_2(NODES_PER_PHASE) bits to store index within phase.
typedef Bit#(2) Phase            // ceil log_2(NUM_OF_PHASES) bits to store phase.

typedef Bit#(9) ServerIndex;       // to show feasibility for upto 512 nodes?

`ifdef MULTI_NIC
typedef 4 NUM_OF_ALTERA_PORTS;
`else
typedef 1 NUM_OF_ALTERA_PORTS;
`endif
typedef Bit#(9) PortIndex;

typedef 2048 CELL_SIZE; //in bits; must be a multiple of BUS_WIDTH

typedef NUM_OF_SERVERS FWD_BUFFER_SIZE;
