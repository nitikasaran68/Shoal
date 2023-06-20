
// This file contains types and helper functions for coordinate math in Shale.

import LFSR::*;
import FIFO::*;
import GetPut::*;
import DefaultValue::*;

// NOTE: If you change these, Header might change as well.
typedef 8 NUM_OF_SERVERS;       // N
typedef 3 NUM_OF_PHASES;        // h
typedef 2 NODES_PER_PHASE;       // (N ** 1/h)
typedef 1 PHASE_SIZE;           // NODES_PER_PHASE - 1. The number of timeslots in each phase.
// NOTE: Add a margin of 1 bit to Coordinate and Phase, because
// we will be doing add & mod operations to increment these.
typedef Bit#(2) Coordinate;     // >= 1 + ceil(log_2(NODES_PER_PHASE)) bits to store each node's index within phase.
typedef Bit#(3) Phase;          // >= 1 + ceil(log_2(NUM_OF_PHASES)) bits to store phase.

typedef Bit#(9) ServerIndex;       // to show feasibility for upto 512 nodes?

// But this doesn't have all the fields mentioned in Shoal paper?!
// NOTE: If you change header format, also change offset values used in Scheduler.
typedef struct {
    ServerIndex src_mac;                // 9 bits
    ServerIndex dst_mac;
    ServerIndex src_ip;
    ServerIndex dst_ip;
    Phase src_mac_phase;                // 3 bits
    Bit#(14) seq_num;                   
    Phase remaining_spraying_hops;      // 3 bits
    Token token1;                       // 12 bits
    Token token2;                       // 12 bits
    Bit#(1) dummy_cell_bit;            
    Bit#(7) spare_padding;              // 7 bits 
} Header deriving(Bits, Eq); // 88 bits

// Before adding CC, header size was equal to the bits_per_cycle (64) for current h/w
// typedef BITS_PER_CYCLE HEADER_SIZE; //size of cell header
// TODO: check that it is totally okay if the header is bigger than this now?
typedef 88 HEADER_SIZE;

// Indices for fields in the header. 
// NOTE: Must be changed if the header structure is changed!
typedef 87 HDR_SRC_MAC_S; typedef 79 HDR_SRC_MAC_E; 
typedef 78 HDR_DST_MAC_S; typedef 70 HDR_DST_MAC_E;
typedef 69 HDR_SRC_IP_S; typedef 61 HDR_SRC_IP_E;
typedef 60 HDR_DST_IP_S; typedef 52 HDR_DST_IP_E;
typedef 51 HDR_SRC_PHASE_S; typedef 49 HDR_SRC_PHASE_E;
typedef 48 HDR_SEQ_NUM_S; typedef 35 HDR_SEQ_NUM_E;
typedef 34 HDR_SPRAY_HOPS_S; typedef 32 HDR_SPRAY_HOPS_E;
typedef 31 HDR_TOKEN_1_S; typedef 20 HDR_TOKEN_1_E;
typedef 19 HDR_TOKEN_2_S; typedef 8 HDR_TOKEN_2_E;
typedef 7 HDR_DUMMY_BIT;


instance DefaultValue#(Header);
    defaultValue = Header {
        src_mac                 : 0,
        dst_mac                 : 0,
        src_ip                  : 0,
        dst_ip                  : 0,
        src_mac_phase           : 0,
        seq_num                 : 0,
        remaining_spraying_hops : 0,
        remote_queue_len        : 0,
        dummy_cell_bit          : 0
    };
endinstance


// Token for congestion control: each token is added to a bucket.
// So token should contain info about which bucket. (dst, spray_hops_left)
typedef struct {
    ServerIndex dst_ip;
    Phase remaining_spraying_hops;
} Token deriving(Bits, Eq);     // 12 bits (9 + 3)

typedef 12 TOKEN_SIZE;

instance DefaultValue#(Header);
    defaultValue = Header {
        dst_ip                  : 0,
        remaining_spraying_hops : 0
    };
endinstance

// Phases are numberd from left-most coordinate (most significant) to right most coordinate.
// So, phase i relates to coordinate i, which is the co-efficient for the (h-i-1)th power of N**1/h. 
// TODO: This is in the opposite order of what Daniel does in simulator. We might change for consistency. 
// TODO: Implement lookup table for powers?
function Coordinate get_coordinate(ServerIndex node, Integer phase);
    Integer x = (valueof(NUM_OF_PHASES) - phase) - 1;  // h - phase_num - 1.
    Integer div = valueof(NODES_PER_PHASE) ** x;       // (N ** x/h)
    get_coordinate = truncate( (node / fromInteger(div)) % fromInteger(valueof(NODES_PER_PHASE)) );
endfunction

// TODO: Add description for func.
function ServerIndex offset_node_in_phase(ServerIndex node, Integer phase, Integer offset);
    Coordinate c = get_coordinate(node, phase);
    Coordinate offset_c = (c + fromInteger(offset)) % fromInteger(valueof(NODES_PER_PHASE));
    Integer x = (valueof(NUM_OF_PHASES) - phase) - 1;  // h - phase_num - 1
    // The offset node ID could be greater or less than this node. Handle sign for diff. 
    if (offset_c < c) 
    begin
        ServerIndex diff = extend(c - offset_c);
        offset_node_in_phase = node - (diff * fromInteger(valueof(NODES_PER_PHASE) ** x));
    end
    else
    begin
        ServerIndex diff = extend(offset_c - c);
        offset_node_in_phase = node + (diff * fromInteger(valueof(NODES_PER_PHASE) ** x));
    end
endfunction

// For direct hops, in each phase we need to find the adjacent node with the phase coordinate matching with the final destination.
// This is the same as the previous function except that here we know the coodinate instead of the offset.
// TODO: consolidate these 2 funcs?
function ServerIndex get_node_with_matching_coordinate(ServerIndex node, Coordinate dst_coord, Integer phase);
    Coordinate c = get_coordinate(node, phase);

    Integer x = (valueof(NUM_OF_PHASES) - phase) - 1;  // h - phase_num - 1
    // The offset node ID could be greater or less than this node. Handle sign for diff. 
    if (dst_coord < c) 
    begin
        ServerIndex diff = extend(c - dst_coord);
        get_node_with_matching_coordinate = node - (diff * fromInteger(valueof(NODES_PER_PHASE) ** x));
    end
    else
    begin
        ServerIndex diff = extend(dst_coord - c);
        get_node_with_matching_coordinate = node + (diff * fromInteger(valueof(NODES_PER_PHASE) ** x));
    end

endfunction

// ---------- Params for NIC -------------

typedef 1024 CELL_SIZE; //in bits; must be a multiple of BUS_WIDTH defined in RingBufferTypes.

typedef NUM_OF_SERVERS FWD_BUFFER_SIZE;

// Module to pick a spraying hop at random, using the LFSR modules.
// Adapted from example on page 308 of BSV ref guide.
// We want 6-bit random numbers, so we will use the 16-bit version of
// LFSR and take the most significant six bits.
// The interface for the random number generator is parameterized on bit
// length. It is a "get" interface, defined in the GetPut package.
// TODO: Change for num of phases value.
typedef Get#(Bit#(3)) RandomHopGenerator;

module mkRandomHopGenerator(Get#(Bit#(3)));
    // First we instantiate the LFSR module
    LFSR#(Bit#(16)) lfsr <- mkLFSR_16 ;
    // Next comes a FIFO for storing the results until needed
    FIFO#(Bit#(3)) gen_fifo <- mkFIFO ;
    
    // A boolean flag for ensuring that we first seed the LFSR module
    Reg#(Bool) starting <- mkReg(True);

    // This rule fires first, and sends a suitable seed to the module.
    rule start (starting);
        starting <= False;
        lfsr.seed('h11);
    endrule: start

    // After that, the following rule runs as often as it can, retrieving
    // results from the LFSR module and enqueing them on the FIFO.
    rule run (!starting);
        gen_fifo.enq(lfsr.value[2:0]);
        lfsr.next;
    endrule: run

    // The interface for mkRn_6 is a Get interface. We can produce this from a
    // FIFO using the fifoToGet function. We therefore don’t need to define any
    // new methods explicitly in this module: we can simply return the produced
    // Get interface as the "result" of this module instantiation.
    return toGet(gen_fifo);
endmodule