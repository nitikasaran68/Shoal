import Clocks::*;
import DefaultValue::*;
import XilinxCells::*;
import GetPut::*;

// NOTE: Set these according to pieo_datatypes.sv !!

typedef 64 PIEO_LIST_SIZE;      // Max number of elements in PIEO queue.
typedef 6 ID_LOG;               // clog2(PIEO_LIST_SIZE) bits to store max flow ID
typedef 16 RANK_LOG;            // bits to store flow rank
typedef 16 TIME_LOG;            // bits to store flow send time
typedef 38 PIEO_ELEMENT_BITS;    // total bits to store a flow in PIEO

typedef 4 CLOG2_NUM_OF_SUBLIST; // bits to store sublist ID

// Struct enqueued and dequeued from PIEO.
typedef struct
{
    Bit#(ID_LOG)    id;
    Bit#(RANK_LOG)  rank;           //init with infinity
    Bit#(TIME_LOG)  send_time;

} PIEOElement deriving(Bits, Eq);


interface PIEOQueue;

    method Action dequeue();
    method Action dequeue_f( Bit#(ID_LOG) id, Bit#(CLOG2_NUM_OF_SUBLIST) sublist_id);
    method PIEOElement get_dequeue_result();

    method Action enqueue(PIEOElement f);
    method Bit#(CLOG2_NUM_OF_SUBLIST) get_enqueue_sublist_id();
    method Bit#(ID_LOG) get_flow_id_moved();
    method Bit#(CLOG2_NUM_OF_SUBLIST) get_flow_id_moved_sublist();

    method Action start();
endinterface

import "BVI" pieo =
module mkPIEOQueue (PIEOQueue);

    default_clock clk();
    default_reset rst();
    
    method dequeue_f(flow_id_in, sublist_id_in) ready (pieo_ready_for_nxt_op_out) enable (dequeue_f_in);

    method dequeue() enable (dequeue_in) ready(pieo_ready_for_nxt_op_out);

    method deq_element_out get_dequeue_result() ready(deq_valid_out);

    method enqueue(f_in) enable(enqueue_f_in) ready(pieo_ready_for_nxt_op_out);

    method f_enqueued_in_sublist_out get_enqueue_sublist_id()  ready( enq_valid_out);

    method flow_id_moved_out get_flow_id_moved()  ready( enq_valid_out);
        
    method flow_id_moved_to_sublist_out get_flow_id_moved_sublist()  ready( enq_valid_out);
        
    method start() ready(pieo_reset_done_out) enable(start);

    schedule (start) SB (dequeue_f, dequeue, enqueue, get_dequeue_result, 
                            get_enqueue_sublist_id, 
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