`ifndef PIEO_DATATYPES
`define PIEO_DATATYPES

package pieo_datatypes;

parameter LIST_SIZE = 9;
parameter NUM_OF_ELEMENTS_PER_SUBLIST = 3; //sqrt(LIST_SIZE)
parameter NUM_OF_SUBLIST = (2*3); //2*NUM_OF_ELEMENTS_PER_SUBLIST

// Bits to store bucket ID (send_time)
parameter ID_LOG = 6;

// Bits to store prev hop of cell.
parameter NODE_ID_LOG = 3;

parameter PHASE_LOG = 3;
parameter TIMESLOT_LOG = 3;

parameter RANK_LOG = 1;

// NOTE: When using with shale, curr_time_in = bitmap for buckets.
// num bits is >= num fwd buckets + 1 (for null bucket).
parameter TIME_LOG = 14;

// This bit should be 0 in every curr_time_in received.
// So we can use this for empty elements, as it also will
// be later in sorted order than any inserted element.
// Numerically equal to the num of fwd token buckets.
parameter NULL_BUCKET = 13;

typedef struct packed
{
    logic [PHASE_LOG-1:0] id;
    logic [TIMESLOT_LOG-1:0] slot; 
    logic [RANK_LOG-1:0] rank;          //init with infinity
    logic [ID_LOG-1:0] send_time;       // Shale interpretation of send_time is ID
    logic [PHASE_LOG-1:0] rem_spray_hops_recvd;   
    logic is_spray;  
} SublistElement;

typedef struct packed
{
    logic [$clog2(NUM_OF_SUBLIST)-1:0] id;
    logic [RANK_LOG-1:0] smallest_rank; //init with infinity
    logic [TIME_LOG-1:0] smallest_send_time; //init with infinity
    logic full;
    logic [$clog2(NUM_OF_SUBLIST/2)-1:0] num;
} PointerElement;

endpackage
`endif
