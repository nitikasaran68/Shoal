
// This file contains types and helper functions for coordinate math in Shale.

import LFSR::*;
import FIFO::*;
import GetPut::*;
import DefaultValue::*;

`include "ConnectalProjectConfig.bsv"

// NOTE: If you change these, Header might change as well.
typedef 16 NUM_OF_SERVERS;       // N
typedef 2 NUM_OF_PHASES;        // h
typedef 4 NODES_PER_PHASE;      // (N ** 1/h)
typedef 3 PHASE_SIZE;           // NODES_PER_PHASE - 1. The number of timeslots in each phase.
typedef 6 EPOCH_SIZE;           // PHASE_SIZE * NUM_OF_PHASES
// NOTE: Add a margin of 1 bit to Coordinate and Phase, because
// we will be doing add & mod operations to increment these.
typedef Bit#(3) Coordinate;     // >= 1 + ceil(log_2(NODES_PER_PHASE)) bits to store each node's index within phase.
typedef Bit#(3) Phase;          // >= 1 + ceil(log_2(NUM_OF_PHASES)) bits to store phase.

typedef Bit#(9) ServerIndex;       // to show feasibility for upto 512 nodes?

`ifdef MULTI_NIC
typedef NUM_OF_SERVERS NUM_OF_ALTERA_PORTS;
`else
typedef 1 NUM_OF_ALTERA_PORTS;
`endif
typedef NUM_OF_SERVERS NUM_OF_SWITCH_PORTS;
typedef Bit#(9) PortIndex;

// In the fwd_buffer, we store cells by phase and send bucket -
// the number of spray hops is the remaining number
// once we forward the packet. Since, we will never receive a
// cell with remaining spray hops > h-1, our fwd buffer will have
// cells with spray hops in [0, h-2].
// NOTE: If this changes, we also need to change PIEO datatypes.
typedef 33 NUM_TOKEN_BUCKETS;                // (N * h) + (1 for final dest)
`ifdef LIMIT_ACTIVE_BUCKETS
// The actual num of active buckets is 15 
// (including one for the final dest.
typedef 17 NUM_ACTIVE_BUCKETS;               // 1 + total number of buckets active at any time
typedef 131070 BUCKET_BITMAP_ALL_FREE;           // (2**NUM_ACTIVE_BUCKETS - 1) - 1 (reserve final dst bucket)
`else
typedef 17 NUM_FWD_TOKEN_BUCKETS;            // (N * (h-1)) + (1 for final dest)
typedef 17 NUM_DIRECT_TOKEN_BUCKETS;         // N + (1 for final dest)
// TODO: define null bkt address, add comments, rename data structures.
`endif
typedef 0 FINAL_DST_BUCKET_IDX;             // 0 
typedef 7 BUCKET_IDX_BITS;
typedef 2 CELLS_PER_BUCKET_HOST;
typedef 1 CELLS_PER_BUCKET_PER_PHASE_FWD;   // Number of cells per bkt per outgoing phase
// typedef CELLS_PER_BUCKET_FWD FWD_BUFFER_SIZE;

typedef 2048 CELL_SIZE; //in bits; must be a multiple of BUS_WIDTH defined in RingBufferTypes.

typedef 64 BITS_PER_CYCLE; //for 10Gbps interface and 156.25MHz clock freq


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

// But this doesn't have all the fields mentioned in Shoal paper?!
// NOTE: If you change header format, also change offset values used in Scheduler.
// TODO: Can reduce this to 64 bits if we cut down ServerIndex size.
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
        dummy_cell_bit          : 0,
        spare_padding           : 0
    };
endinstance


// Token for congestion control: each token is added to a bucket.
// So token should contain info about which bucket. (dst, spray_hops_left)
typedef struct {
    ServerIndex dst_ip;
    Phase remaining_spraying_hops;
} Token deriving(Bits, Eq);     // 12 bits (9 + 3)

typedef 12 TOKEN_SIZE;

instance DefaultValue#(Token);
    defaultValue = Token {
        dst_ip                  : 0,
        remaining_spraying_hops : 0
    };
endinstance

// Phases are numberd from right-most coordinate (least significant) to left most coordinate.
// So, phase i relates to coordinate i, which is the co-efficient for the i-th power of N**1/h. 
// TODO: Implement lookup table for powers?
function Coordinate get_coordinate(ServerIndex node, Integer phase);
    Integer div = valueof(NODES_PER_PHASE) ** phase;       // (N ** x/h)
    get_coordinate = truncate( (node / fromInteger(div)) % fromInteger(valueof(NODES_PER_PHASE)) );
endfunction

// TODO: Add description for func.
function ServerIndex offset_node_in_phase(ServerIndex node, Integer phase, Integer offset);
    Coordinate c = get_coordinate(node, phase);
    Coordinate offset_c = (c + fromInteger(offset)) % fromInteger(valueof(NODES_PER_PHASE));
    // The offset node ID could be greater or less than this node. Handle sign for diff. 
    if (offset_c < c) 
    begin
        ServerIndex diff = extend(c - offset_c);
        offset_node_in_phase = node - (diff * fromInteger(valueof(NODES_PER_PHASE) ** phase));
    end
    else
    begin
        ServerIndex diff = extend(offset_c - c);
        offset_node_in_phase = node + (diff * fromInteger(valueof(NODES_PER_PHASE) ** phase));
    end
endfunction

// For direct hops, in each phase we need to find the adjacent node with the phase coordinate matching with the final destination.
// This is the same as the previous function except that here we know the coodinate instead of the offset.
// TODO: consolidate these 2 funcs? 
// This function returns the timeslot of the phase in which the node is connected to this matching node.
function Coordinate get_timeslot_with_matching_coordinate(ServerIndex node, Coordinate dst_coord, Integer phase);
    Coordinate c = get_coordinate(node, phase);

    // Integer x = (valueof(NUM_OF_PHASES) - phase) - 1;  // h - phase_num - 1
    // The dst node ID could be greater or less than this node.
    if (dst_coord < c) 
    begin
        // ServerIndex diff = extend(c - dst_coord);
        // get_node_with_matching_coordinate = node - (diff * fromInteger(valueof(NODES_PER_PHASE) ** x));
        Coordinate timeslot = (fromInteger(valueof(NODES_PER_PHASE)-1) - c) + dst_coord;
        get_timeslot_with_matching_coordinate = timeslot;
    end
    else
    begin
        // ServerIndex diff = extend(dst_coord - c);
        // get_node_with_matching_coordinate = node + (diff * fromInteger(valueof(NODES_PER_PHASE) ** x));
        Coordinate timeslot = (dst_coord - c - 1);
        get_timeslot_with_matching_coordinate = timeslot;
    end

endfunction



// Get index in fwd buffer from the bucket containing the 
// final destination and the number of remaining hops.
// This function converts the 2D index (bucket) to a scalar
// index by treating the number of spray hops as rows and
// destinations as columns. We add a 1 to this idx because
// we mark the final dest bucket as idx 0.
function Bit#(BUCKET_IDX_BITS) get_fwd_bucket_from_tkn(Token tkn);
    let d = tkn.dst_ip;     // dest idx: 0 to N-1
    let h = tkn.remaining_spraying_hops;
    // For each dst, spray hops can be from 0 to H-1. 
    // H remaining spray hops implies a local flow.
    Bit#(BUCKET_IDX_BITS) idx = extend(h) * fromInteger(valueof(NUM_OF_SERVERS));
    get_fwd_bucket_from_tkn = idx + truncate(d) + 1;
endfunction

function Bit#(BUCKET_IDX_BITS) get_fwd_bucket_idx(ServerIndex d, Phase h);
    // For each dst, spray hops can be from 0 to H-1. 
    // H remaining spray hops implies a local flow.
    Bit#(BUCKET_IDX_BITS) idx = extend(h) * fromInteger(valueof(NUM_OF_SERVERS));
    get_fwd_bucket_idx = idx + truncate(d) + 1;
endfunction

function Bit#(BUCKET_IDX_BITS) get_direct_buffer_idx_from_bucket(
                                            Bit#(BUCKET_IDX_BITS) bkt);
    if (bkt == fromInteger(valueOf(FINAL_DST_BUCKET_IDX)))
        get_direct_buffer_idx_from_bucket = bkt;
    else
        get_direct_buffer_idx_from_bucket =
            ((bkt - 1) % fromInteger(valueof(NUM_OF_SERVERS))) + 1;
endfunction

function Token get_token_from_bucket_idx(Bit#(BUCKET_IDX_BITS) bucket_idx);
    Token tkn;
    tkn.dst_ip = extend((bucket_idx - 1) % fromInteger(valueof(NUM_OF_SERVERS)));
    Bit#(BUCKET_IDX_BITS) spray_hops = (bucket_idx - 1 - truncate(tkn.dst_ip)) /
                                        fromInteger(valueof(NUM_OF_SERVERS));
    tkn.remaining_spraying_hops = truncate(spray_hops);
    get_token_from_bucket_idx = tkn;
endfunction

function Integer phase_to_int(Phase p);
    Integer phase_int = 0;
    if (valueof(NUM_OF_PHASES) > 1 && p == 1) phase_int = 1;
    if (valueof(NUM_OF_PHASES) > 2 && p == 2) phase_int = 2;
    if (valueof(NUM_OF_PHASES) > 3 && p == 3) phase_int = 3;
    phase_to_int = phase_int;
endfunction

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