import Clocks::*;
import DefaultValue::*;
import XilinxCells::*;
import GetPut::*;

import ShaleUtil::*;

// NOTE: Set these according to pieo_datatypes.sv !!

// TODO: PIEO_LIST_SIZE should be the same as max num fwd token buckets * buckt size

typedef 9 PIEO_LIST_SIZE;       // Max number of elements in PIEO queue - round up to perfect square.
typedef 4 ID_LOG;               // clog2(PIEO_LIST_SIZE) bits to store max flow ID
typedef 3 NODE_ID_LOG;        // Bits to store prev hop of cell.
// typedef 3 PHASE_LOG;
// typedef 3 TIMESLOT_LOG;
typedef 4 RANK_LOG;             // bits to store flow rank
typedef 6 TIME_LOG;            // bits to store flow send time
typedef 17 PIEO_ELEMENT_BITS;   // bits to store a flow in PIEO
typedef 15 PIEO_NULL_ID;        // 2**ID_LOG - 1

typedef 3 CLOG2_NUM_OF_SUBLIST; // bits to store sublist ID

// Struct enqueued and dequeued from PIEO.
typedef struct
{
    Phase    prev_hop_phase;                // For FWD cells, hop this cell was recvd from.
    Coordinate prev_hop_slot;
    Bit#(RANK_LOG)  rank;                   // init with infinity
    Bit#(ID_LOG)  id;
    Phase rem_spraying_hops_recvd;
} PIEOElement deriving(Bits, Eq);


interface PIEOQueue;

    method Action dequeue(Bit#(TIME_LOG) curr_time);
    method Action dequeue_f( Bit#(ID_LOG) id, Bit#(CLOG2_NUM_OF_SUBLIST) sublist_id);
    method PIEOElement get_dequeue_result();

    method Action enqueue(PIEOElement f);
    method Bit#(CLOG2_NUM_OF_SUBLIST) get_enqueue_sublist_id();
    method Bit#(ID_LOG) get_flow_id_moved();
    method Bit#(CLOG2_NUM_OF_SUBLIST) get_flow_id_moved_sublist();

    method Action reset_queue();
endinterface

import "BVI" pieo =
module mkPIEOQueue (PIEOQueue);

    // define an input clock clk, with verilog port clk, and set this to be the default
    default_clock clk(clk);
    default_reset rst();

    port start = 1;

    // Fix current time input for dequeue to be 0, since eligibility is binary for Shale.
    // Send time for each flow will be 1 when not eligible, and 0 otherwise.
    // port curr_time_in = 0;
    
    method dequeue_f(flow_id_in, sublist_id_in) ready (pieo_ready_for_nxt_op_out) enable (dequeue_f_in);

    method dequeue(curr_time_in) enable (dequeue_in) ready(pieo_ready_for_nxt_op_out);

    method deq_element_out get_dequeue_result() ready(deq_valid_out);

    method enqueue(f_in) enable(enqueue_f_in) ready(pieo_ready_for_nxt_op_out);

    method f_enqueued_in_sublist_out get_enqueue_sublist_id()  ready( enq_valid_out);

    // TODO: add these two methods for deq as well
    method flow_id_moved_out get_flow_id_moved()  ready( enq_valid_out);
        
    method flow_id_moved_to_sublist_out get_flow_id_moved_sublist()  ready( enq_valid_out);
    
    method reset_queue() enable(rst);

    schedule (reset_queue) SB (dequeue_f, dequeue, enqueue, get_dequeue_result, 
                            get_enqueue_sublist_id, 
                            get_flow_id_moved, 
                            get_flow_id_moved_sublist);
    schedule reset_queue C reset_queue;

    schedule (  get_dequeue_result) CF (get_enqueue_sublist_id, 
                                        get_flow_id_moved, 
                                        get_flow_id_moved_sublist);
                                        
    schedule (dequeue_f, dequeue, enqueue) C (dequeue_f, dequeue, enqueue);
    schedule (get_dequeue_result, 
                get_enqueue_sublist_id, 
                get_flow_id_moved, 
                get_flow_id_moved_sublist) SB (dequeue_f, dequeue, enqueue);

    schedule (  get_dequeue_result) CF (get_enqueue_sublist_id, 
                                        get_flow_id_moved, 
                                        get_flow_id_moved_sublist);

    schedule (  get_enqueue_sublist_id) CF (get_dequeue_result, 
                                            get_flow_id_moved, 
                                            get_flow_id_moved_sublist);

    schedule (  get_flow_id_moved) CF ( get_dequeue_result, 
                                        get_enqueue_sublist_id, 
                                        get_flow_id_moved_sublist);

    schedule (  get_flow_id_moved_sublist) CF (get_dequeue_result, 
                                                get_enqueue_sublist_id, 
                                                get_flow_id_moved);
    schedule get_dequeue_result C get_dequeue_result;
    schedule get_enqueue_sublist_id C get_enqueue_sublist_id;
    schedule get_flow_id_moved C get_flow_id_moved;
    schedule get_flow_id_moved_sublist C get_flow_id_moved_sublist;
    
endmodule