import Clocks::*;
import DefaultValue::*;
import XilinxCells::*;
import GetPut::*;

import ShaleUtil::*;

`include "ConnectalProjectConfig.bsv"

// NOTE: Set these according to pieo_datatypes.sv !!
// typedef 3 PHASE_LOG;
// typedef 3 TIMESLOT_LOG;
typedef 1 RANK_LOG;                 // bits to store flow rank
typedef 14 TIME_LOG;                 // bits to store flow send time: number of fwd buckets + 1
typedef 63 PIEO_NULL_ID;            // 2**ID_LOG - 1
// typedef 4 NUM_OF_SUBLIST;        // 2 * root( PIEO_LIST_SIZE)
// typedef 2 CLOG2_NUM_OF_SUBLIST;     // clog2(NUM_OF_SUBLIST)

// Struct enqueued and dequeued from PIEO.
// TODO: Is there any way to shorten this?
typedef struct
{
    Phase    prev_hop_phase;                // For FWD cells, hop this cell was recvd from.
    Coordinate prev_hop_slot;
    Bit#(RANK_LOG)  rank;                   // init with infinity
    Bit#(BUCKET_IDX_BITS)  id;
    Phase rem_spraying_hops_recvd;
    Bit#(1) is_spray;
} PIEOElement deriving(Bits, Eq);


interface PIEOQueue;

    method Action dequeue(Bit#(TIME_LOG) curr_time);
    method PIEOElement get_dequeue_result();
    method Action enqueue(PIEOElement f);

    // method Action dequeue_f( Bit#(ID_LOG) id, Bit#(CLOG2_NUM_OF_SUBLIST) sublist_id);
    // method Bit#(CLOG2_NUM_OF_SUBLIST) get_enqueue_sublist_id();
    // method Bit#(ID_LOG) get_flow_id_moved();
    // method Bit#(CLOG2_NUM_OF_SUBLIST) get_flow_id_moved_sublist();

    method Action reset_queue();
endinterface

import "BVI" pieo =
module mkPIEOQueue#(Integer verbose) (PIEOQueue);

    parameter verbose = verbose;

    // define an input clock clk, with verilog port clk, and set this to be the default
    default_clock clk(clk);
    default_reset rst();

    port start = 1;
    
    method dequeue(curr_time_in) enable (dequeue_in) ready(pieo_ready_for_nxt_op_out);

    method deq_element_out get_dequeue_result() ready(deq_valid_out);

    method enqueue(f_in) enable(enqueue_f_in) ready(pieo_ready_for_nxt_op_out);

    method reset_queue() enable(rst);

    // method dequeue_f(flow_id_in, sublist_id_in) ready (pieo_ready_for_nxt_op_out) enable (dequeue_f_in);

    // method f_enqueued_in_sublist_out get_enqueue_sublist_id()  ready( enq_valid_out);

    // NOTE: similarly two methods for deq as well
    // method flow_id_moved_out get_flow_id_moved()  ready( enq_valid_out);
        
    // method flow_id_moved_to_sublist_out get_flow_id_moved_sublist()  ready( enq_valid_out);
    

    schedule (reset_queue) SB (dequeue, enqueue, get_dequeue_result) ;
    
    schedule reset_queue C reset_queue;
                      
    schedule (dequeue, enqueue) C (dequeue, enqueue);

    schedule (get_dequeue_result) SB (dequeue, enqueue);

    // This method is simply a read - no conflict!
    schedule get_dequeue_result CF get_dequeue_result;

    // schedule  dequeue_f C (dequeue, enqueue, dequeue_f);

    // schedule (get_enqueue_sublist_id, 
    //         get_flow_id_moved, 
    //         get_flow_id_moved_sublist) SB (dequeue, enqueue, dequeue_f);

    // schedule (  get_dequeue_result) CF (get_enqueue_sublist_id, 
    //                                     get_flow_id_moved, 
    //                                     get_flow_id_moved_sublist);

    // schedule (  get_enqueue_sublist_id) CF (get_dequeue_result, 
    //                                         get_flow_id_moved, 
    //                                         get_flow_id_moved_sublist);

    // schedule (  get_flow_id_moved) CF ( get_dequeue_result, 
    //                                     get_enqueue_sublist_id, 
    //                                     get_flow_id_moved_sublist);

    // schedule (  get_flow_id_moved_sublist) CF (get_dequeue_result, 
    //                                             get_enqueue_sublist_id, 
    //                                             get_flow_id_moved);

    // schedule (reset_queue) SB (dequeue_f,
    //                             get_enqueue_sublist_id, 
    //                             get_flow_id_moved, 
    //                             get_flow_id_moved_sublist);

    // schedule get_enqueue_sublist_id C get_enqueue_sublist_id;
    // schedule get_flow_id_moved C get_flow_id_moved;
    // schedule get_flow_id_moved_sublist C get_flow_id_moved_sublist;
    
endmodule