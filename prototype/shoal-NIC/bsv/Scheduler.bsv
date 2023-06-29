import Vector::*;
import FIFO::*;
import FIFOF::*;
import ClientServer::*;
import SpecialFIFOs::*;
import BRAM::*;
import GetPut::*;
import DefaultValue::*;
import Clocks::*;
import PIEOQueue::*;

import Params::*;
import ShaleUtil::*;
import SchedulerTypes::*;
import RingBufferTypes::*;
import RingBuffer::*;
import CellGenerator::*;
import Mac::*;

`include "ConnectalProjectConfig.bsv"

interface Scheduler;
    // Responses to stats request.
    interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
        time_slots_res;
	interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
        received_host_pkt_res;
	interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
        received_wrong_dst_pkt_res;
	interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(72)))
        latency_res;
    interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
        max_fwd_buffer_length_res;
    // interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     sent_host_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     sent_fwd_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     received_fwd_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     received_corrupted_pkt_res;

    // Stats
    method Action timeSlotsCount();
	method Action receivedHostPktCount();
	method Action receivedWrongDstPktCount();
    method Action latency();
    method Action max_fwd_buffer_length();
    // method Action sentHostPktCount();
	// method Action sentFwdPktCount();
    // method Action receivedFwdPktCount();
	// method Action receivedCorruptedPktCount();

    method Action start(ServerIndex first_host_index, Bit#(8) t);
    method Action stop();
endinterface

module mkScheduler#(Mac mac, Vector#(NUM_OF_ALTERA_PORTS, CellGenerator) cell_generator,
                Clock pcieClock, Reset pcieReset) (Scheduler);

    Bool verbose = True;

    Clock defaultClock <- exposeCurrentClock();
    Reset defaultReset <- exposeCurrentReset();

    Reg#(Bit#(8)) timeslot_len <- mkReg(0);

    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(1))) start_flag <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(1))) running <- replicateM(mkReg(0));

    // Clock for stats.
    Reg#(Bit#(72)) current_time <- mkReg(0);
    rule clk;
        current_time <= current_time + 1;
    endrule

    /*------------------------------------------------------------------------------*/

                                /* Init Path */
            /* Takes 1 cycles to set coordinates + schedulte table. */

    /*------------------------------------------------------------------------------*/

    // Node ID for each port.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex)) host_index
        <- replicateM(mkReg(maxBound));

    // The schedule is a 3D matrix, indexed by host_idx, phase, slot_within_phase.
    // The table need only contain schedules for the nodes on this device.
	Vector#(NUM_OF_ALTERA_PORTS, Vector#(NUM_OF_PHASES, Vector#(PHASE_SIZE, Reg#(ServerIndex))))
        schedule_table <- replicateM(replicateM(replicateM(mkReg(0))));

    // For Tx. Destination node of the next local flow we will send out, when there's an opportunity.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex)) 
        next_local_flow_idx <- replicateM(mkReg(0));

    // Once on receiving start signal (and therefor host_index), set coordinates and load schedule into table for each port.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(1))) is_schedule_set <- replicateM(mkReg(0));
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        rule populate_schedule_table (start_flag[i] == 1 && is_schedule_set[i] == 0);
            for (Integer phase = 0; phase < valueof(NUM_OF_PHASES); phase = phase + 1)  // Phase
            begin
                for (Integer j = 0; j < valueof(PHASE_SIZE); j = j + 1)                 // Slot within phase
                    schedule_table[i][phase][j] <= offset_node_in_phase(host_index[i], phase, j + 1);
            end

            // Init round robin for local flows to send.
            next_local_flow_idx[i] <= fromInteger((i + 1) % valueof(NUM_OF_SERVERS));

            is_schedule_set[i] <= 1;
            
            // State is initialized, so now this port may run rx / tx / reconfig.
            if (verbose)
                $display("[SCHED %d] Init finished at abs time %d",host_index[i], current_time);
            running[i] <= 1;
        endrule
    end

    /* ---------------- Stats ----------------- */
	Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) time_slots_fifo
	        <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) received_host_pkt_fifo
	        <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	
	Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) received_wrong_dst_pkt_fifo
	        <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(72))) latency_fifo
	        <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
    Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) buffer_length_fifo
	        <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
    // Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) sent_host_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) sent_fwd_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
    // Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) received_fwd_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) received_corrupted_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));

	Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64))) num_of_time_slots_used_reg
        <- replicateM(mkReg(0));
	Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
        num_of_host_pkt_received_reg <- replicateM(mkReg(0));
	Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
        num_of_wrong_dst_pkt_received_reg <- replicateM(mkReg(0));
	Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(72)))
        latency_reg <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
        max_fwd_buffer_length_reg <- replicateM(mkReg(0));
    // Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64))) host_pkt_transmitted_reg
    //     <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64))) non_host_pkt_transmitted_reg
    //     <- replicateM(mkReg(0));
    // Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
    //     num_of_fwd_pkt_received_reg <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
    //     num_of_corrupted_pkt_received_reg <- replicateM(mkReg(0));

/*------------------------------------------------------------------------------*/

                                /* Reconfigure Path */
            /* 1 cycle to reconfigure, executes every timeslot_len cycles */

/*------------------------------------------------------------------------------*/

    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(8)))
        clock_within_timeslot <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Coordinate))
        current_timeslot <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Phase))
        current_phase <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
        current_epoch <- replicateM(mkReg(0));

    // Tx neighbor.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex))
        current_neighbor <- replicateM(mkReg(maxBound));

    // Signal FIFO to trigger choice of cell to send.
    Vector#(NUM_OF_ALTERA_PORTS, Vector#(NUM_OF_PHASES, Vector#(PHASE_SIZE, FIFO#(void))))
        choose_buffer_to_send_from_fifo <- replicateM(replicateM(replicateM(mkFIFO)));

    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        
        // Here, clock_within_timeslot counts cycles up to the timeslot length, repeatedly. At the start of
        // each time slot, it updates the neighbor acc to the schedule, and enqueues a signal to choose 
        // the tx cell for the time slot. At the end of a timeslot, it increments the time slot and optionally 
        // the phase & epoch.
        // TODO: In Shoal, the timeslot is incremented when the clock is 0, and the neighbor is also reset.
        // But because the new timeslot will reflect in the next clock cycle, it correctly sets neighbor for
        // current timeslot. BUT if the time_slot value is used by other rules (it isn't currently), 
        // this will be +1 of the current time slot? 
        rule set_current_timeslot (running[i] == 1 && is_schedule_set[i] == 1);
        
            if (clock_within_timeslot[i] == 0)        // New timeslot
                begin
                    // Set neighbor using current timeslot value.
                    ServerIndex d = schedule_table[host_index[i]]
                                [current_phase[i]][current_timeslot[i]];
                    current_neighbor[i] <= d;

                    num_of_time_slots_used_reg[i] <= num_of_time_slots_used_reg[i] + 1;

                    if (verbose && host_index[i] == 0)
                        $display("[SCHED %d] New time slot at abs time %d. Set phase = %d timeslot = %d dst = %d ",
                            host_index[i], current_time, current_phase[i], current_timeslot[i], d);

                    // Choose whether to send local or remote cells in this timeslot.
                    choose_buffer_to_send_from_fifo[i][current_phase[i]][current_timeslot[i]].enq(?);
                end
            
            // Timelsot ends after current cycle.
            if (clock_within_timeslot[i] == timeslot_len - 1)
            begin
                clock_within_timeslot[i] <= 0;

                // Updates timeslot, phase, and epoch values for next clock cycle.
                current_timeslot[i] <= (current_timeslot[i] + 1)
                        % fromInteger(valueof(PHASE_SIZE));
                // New phase - check old value of current_timeslot.
                if (current_timeslot[i] == fromInteger(valueof(PHASE_SIZE)-1)) 
                begin                  
                    current_phase[i] <= (current_phase[i] + 1) 
                        % fromInteger(valueof(NUM_OF_PHASES));
                    // New epoch - check old value of phase.
                    if (current_phase[i] == fromInteger(valueof(NUM_OF_PHASES)-1))                      
                        current_epoch[i] <= current_epoch[i] + 1;
                end
            end
            else
                clock_within_timeslot[i] <= clock_within_timeslot[i] + 1;
        endrule
    end

    /*------------------------------------------------------------------------------*/

                                /* Tx Path */

    /*------------------------------------------------------------------------------*/

    // BRAM to store cells to forward.
    // TODO: Don't need N fwd buffers per port, only EPOCH_SIZE.
    // but then need some way to idx by nbr host_idx. May need to make an interacing module.
    Vector#(NUM_OF_ALTERA_PORTS, Vector#(NUM_OF_PHASES, Vector#(PHASE_SIZE,
        RingBuffer#(ReadReqType, ReadResType, WriteReqType))))
            fwd_buffer <- replicateM(replicateM(replicateM
                (mkRingBuffer(valueof(FWD_BUFFER_SIZE), valueof(CELL_SIZE)))));
    
    // State to determine next cell to send.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(BufType))
        buffer_to_send_from <- replicateM(mkReg(HOST));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex))
        host_flow_to_send <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, FIFO#(ReadResType))
        cell_to_send_fifo <- replicateM(mkSizedFIFO(2));
    
    // Header for outgoing cells. 
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(HEADER_SIZE)))
        curr_header <- replicateM(mkReg(0));

    // Tx rules for each port.
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin


        // Tx rules for each destination node. At most one req_*_cell rule gets fired per timeslot,
        // depending on buffer_to_send_from type. These rules only get fired on the third clock cycle
        // of each timeslot. It is obvious that these should only fire on a single clock cycle of the timeslot.
        // This is the 3nd cycle because set_current_timeslot and choose_buffer_to_send_from take 1 cycle each to execute.
        // TODO: Check whether bluespec "rules" execute in a single clock cycle? (BSV ref 6.2.2)
        // TODO: Why not just have signal FIFOs to fire these rules, instead of checking clock_within_timeslot ??
        
        // A rule to get local cells for each possible destination, when sending local traffic.
        for (Integer j = 0; j < valueof(NUM_OF_SERVERS); j = j + 1)
        begin
            // Get local cells for next local flow to send.
            rule request_local_cell (running[i] == 1 && clock_within_timeslot[i] == 2
                            && buffer_to_send_from[i] == HOST
                            && host_flow_to_send[i] == fromInteger(j));        
                cell_generator[i].host_cell_req[j].put(?);
            endrule

            // This rule may multiple times, as for each host cell request, the generator responds with
            // packets of size BUS_WIDTH. So, each request triggers this rule CELL_SIZE/BUS_WIDTH times.
            // Same for FWD cell requests.
            // TODO: Should we check that all packets of the cell are transmitted to the same (correct) neighbor.
            rule get_local_cell;
                let d <- cell_generator[i].host_cell_res[j].get;          
                cell_to_send_fifo[i].enq(d);
            endrule
        end
        
        // A rule to get FWD cells for each possible neighbor based on timeslot & phase, when sending FWD traffic.
        for (Integer j = 0; j < valueof(NUM_OF_PHASES); j = j + 1)
        begin
            for (Integer k = 0; k < valueof(PHASE_SIZE); k = k + 1)
            begin
                // Get cell to send from FWD buffer of current neighbor.
                rule request_fwd_cell (running[i] == 1 && clock_within_timeslot[i] == 2
                                && buffer_to_send_from[i] == FWD
                                && current_phase[i] == fromInteger(j) && current_timeslot[i] == fromInteger(k));              
                    fwd_buffer[i][j][k].read_request.put(makeReadReq(READ));
                endrule

                rule get_fwd_cell;
                    let d <- fwd_buffer[i][j][k].read_response.get;
                    cell_to_send_fifo[i].enq(d);
                endrule
            end
        end

        // Stat collection for buffer lengths
        rule get_max_buffer_len (running[i] == 1);
            Bit#(64) total_fwd_buf_len = 0;
            for (Integer j = 0; j < valueof(NUM_OF_PHASES); j = j + 1)
            begin
                for (Integer k = 0; k < valueof(PHASE_SIZE); k = k + 1)
                begin
                    total_fwd_buf_len = total_fwd_buf_len + 
                                        fwd_buffer[i][j][k].elements;    
                end
            end
            if(total_fwd_buf_len > max_fwd_buffer_length_reg[i])
                max_fwd_buffer_length_reg[i] <= total_fwd_buf_len;
        endrule

        rule req_dummy_cell (running[i] == 1 && clock_within_timeslot[i] == 2
                        && buffer_to_send_from[i] == DUMMY);
            if (verbose)
                $display("[SCHED %d] Requesting dummy cell", host_index[i]);
            cell_generator[i].dummy_cell_req.put(?);
        endrule

        rule get_dummy_cell;
            let d <- cell_generator[i].dummy_cell_res.get;
            if (verbose)
                $display("[SCHED %d] Enqueuing dummy cell", host_index[i]);
            cell_to_send_fifo[i].enq(d);
        endrule

        // Rule to actually send by forwarding cell to lower layers. Only fires once we enq to cell_to_send_fifo.
        rule send_cell (running[i] == 1);
            let d <- toGet(cell_to_send_fifo[i]).get;

            Bit#(HEADER_SIZE) h = curr_header[i];
            if (d.data.sop == 1)
            begin
                Integer s = valueof(BUS_WIDTH) - 1;
                Integer e = valueof(BUS_WIDTH) - valueof(HEADER_SIZE);
                h = d.data.payload[s:e];

                // Set mac layer information in header, once per cell sent.
                h[valueof(HDR_SRC_MAC_S):valueof(HDR_SRC_MAC_E)] = host_index[i];         // src_mac
                h[valueof(HDR_DST_MAC_S):valueof(HDR_DST_MAC_E)] = current_neighbor[i];   // dst_mac
                h[valueof(HDR_SRC_PHASE_S):valueof(HDR_SRC_PHASE_E)] = current_phase[i];  // src_mac_phase
                // TODO: Was Vishal doing this via a | operation:   
                // Bit#(HEADER_SIZE) x = {host_index[i], current_neighbor[i], '0};
                // h = d.data.payload[s:e] | x;

                curr_header[i] <= h;
            end

            ServerIndex dst_ip = h[valueof(HDR_DST_IP_S):valueof(HDR_DST_IP_E)];
            Bit#(14) seq_num = h[valueof(HDR_SEQ_NUM_S):valueof(HDR_SEQ_NUM_E)];

            // NOTE: Change this if header size changes!
            // TODO: hard-coded index
            if (d.data.eop == 1)
                d.data.payload = {h, h, h, h, h, current_time};
            else
                d.data.payload = {h, h, h, h, h, h[87:16]};

            // Put to MAC interface.
            mac.mac_tx_write_req[i].put(d.data);

            if (verbose)
                $display("[SCHED %d] Sending cell dst = %d seq_num=%d sop = %d eop = %d to neighbor %d at time %d",
                    host_index[i], dst_ip, seq_num ,d.data.sop, d.data.eop, current_neighbor[i], current_time);
        endrule
    end

/*------------------------------------------------------------------------------*/

                                /* Rx Path */

/*------------------------------------------------------------------------------*/

    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex))
        curr_src_mac <- replicateM(mkReg(fromInteger(valueof(NUM_OF_SERVERS))));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex))
        curr_dst_mac <- replicateM(mkReg(fromInteger(valueof(NUM_OF_SERVERS))));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex))
        curr_src_ip <- replicateM(mkReg(fromInteger(valueof(NUM_OF_SERVERS))));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex))
        curr_dst_ip <- replicateM(mkReg(fromInteger(valueof(NUM_OF_SERVERS))));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(1)))
        curr_dummy_bit <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(HEADER_SIZE)))
        curr_rx_header <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(1)))
        curr_corrupted_cell <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(16)))
        curr_cell_size <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Phase)) 
        curr_remaining_spray_hops <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Phase)) 
        curr_src_mac_phase <- replicateM(mkReg(0));

    // Module to pick spraying hops.
    RandomHopGenerator rng_hop <-mkRandomHopGenerator;

    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        rule receive_cell (running[i] == 1);
            // Get from MAC interface
            let d <- mac.mac_rx_read_res[i].get;

            ServerIndex src_mac = curr_src_mac[i];
            ServerIndex dst_mac = curr_dst_mac[i];
            ServerIndex src_ip = curr_src_ip[i];
            ServerIndex dst_ip = curr_dst_ip[i];
            Phase src_mac_phase = curr_src_mac_phase[i];
            Phase remaining_spraying_hops = curr_remaining_spray_hops[i];
            Bit#(1) dummy_bit = curr_dummy_bit[i];

            Bit#(HEADER_SIZE) hd = curr_rx_header[i];

            Bit#(1) corrupted_cell = curr_corrupted_cell[i];

            // All the indicies assume BUS_WIDTH of 512; change them if you change
            // BUS_WIDTH

            Bit#(16) cell_size_cnt = curr_cell_size[i];

            if (d.sop == 1)
            begin
                // TODO: hard-coded indices
                // For first chunk (BUS_WIDTH size) of cell, extract header
                hd = d.payload[511:424];
                curr_rx_header[i] <= hd;

                corrupted_cell = 0;
                src_mac = hd[valueof(HDR_SRC_MAC_S): valueof(HDR_SRC_MAC_E)];
                dst_mac = hd[valueof(HDR_DST_MAC_S): valueof(HDR_DST_MAC_E)];
                src_ip = hd[valueof(HDR_SRC_IP_S): valueof(HDR_SRC_IP_E)];
                dst_ip = hd[valueof(HDR_DST_IP_S): valueof(HDR_DST_IP_E)];
                src_mac_phase = hd[valueof(HDR_SRC_PHASE_S): valueof(HDR_SRC_PHASE_E)];
                remaining_spraying_hops = hd[valueof(HDR_SPRAY_HOPS_S):valueof(HDR_SPRAY_HOPS_E)];
                dummy_bit = hd[valueof(HDR_DUMMY_BIT)];

                curr_src_mac[i] <= src_mac;
                curr_dst_mac[i] <= dst_mac;
                curr_src_ip[i] <= src_ip;
                curr_dst_ip[i] <= dst_ip;
                curr_src_mac_phase[i] <= src_mac_phase;
                curr_remaining_spray_hops[i] <= remaining_spraying_hops;
                curr_dummy_bit[i] <= dummy_bit;

                

                if (dst_mac != host_index[i])
                begin
                    if (verbose)
                        $display("[SCHED %d] GOT WRONG PKT: src_mac=%d dst_mac=%d src_mac_phase=%d", 
                                    host_index[i], src_mac, dst_mac, src_mac_phase);
                    corrupted_cell = 1;
                    num_of_wrong_dst_pkt_received_reg[i]
                        <= num_of_wrong_dst_pkt_received_reg[i] + 1;
                end

                cell_size_cnt = fromInteger(valueof(BUS_WIDTH));
                curr_cell_size[i] <= cell_size_cnt;
            end
            else
            begin
                cell_size_cnt = cell_size_cnt + fromInteger(valueof(BUS_WIDTH));
                curr_cell_size[i] <= cell_size_cnt;
            end

            // ------------- Check for corruption + deliver, completely taken from Shoal ------------
            // TODO: Fix all hard-coded indices, define these in RingBufferTypes? 

            // TODO: Assumes header size of 64 and BUS_WIDTH of 512, fix for different values of these.
            // Last header chunk is of size BUS_WIDTH % header size.
            Bit#(BUS_WIDTH) c = {hd, hd, hd, hd, hd, hd[87:16]};

            if (corrupted_cell == 0)
            begin
                if (d.eop == 0)
                begin
                // For every packet except last, cell is set to repeated header blocks (in tx path).
                // If this doesn't match what is receied, packet is corrupted.
                    if (d.payload != c)
                    begin
                        corrupted_cell = 1;
                        // num_of_corrupted_pkt_received_reg[i]
                        //     <= num_of_corrupted_pkt_received_reg[i] + 1;
                    end
                end
                else if (d.eop == 1)
                begin
                // For last packet, only the last (BUS_WIDTH % header size) bits will be set to time of sending instead of header, check everything else.
                // TODO: hard-coded indices.
                    if (cell_size_cnt != fromInteger(valueof(CELL_SIZE))
                        || d.payload[511:72] != c[511:72])
                    begin
                        corrupted_cell = 1;
                        if(verbose)
                            $display("[SCHED %d] GOT CORRUPTED PKT payload=%x   c=%x", host_index[i], d.payload[511:64], c[511:64]);
                        // num_of_corrupted_pkt_received_reg[i]
                        //     <= num_of_corrupted_pkt_received_reg[i] + 1;
                    end

                    if (corrupted_cell == 0 && dummy_bit == 0)
                    begin
                        // Collect latency stats for the cell for a single hop at MAC layer (time_recvd - time_sent).
                        // This one-way delay could be skewed for different clocks at different FPGA boards without clock sync. 
                        // TODO: Shoal was counting corrupted packets towards latency stats as well? Also, only one data point, not agg.
                        // TODO: hard-coded indices.
                        Bit#(72) send_time = d.payload[71:0];
                        Bit#(72) latency = current_time - send_time;
                        if (send_time != 0 && latency_reg[i] == 0)
                            latency_reg[i] <= latency;

                        if (verbose)
                        begin
                            // hard-code indices.
                            Bit#(14) seq_num = hd[valueof(HDR_SEQ_NUM_S):valueof(HDR_SEQ_NUM_E)];
                            $display("[SCHED %d] Received cell with src_mac=%d; src_ip=%d; dst_ip=%d; src_mac_phase=%d; seq_num=%d latency=%d cycles at phase=%d; send_time=%d; recv_time=%d",
                                        host_index[i], src_mac, src_ip, dst_ip, src_mac_phase, seq_num, latency, current_phase[i], send_time, current_time);
                        end
                        if (dst_ip == host_index[i])
                            num_of_host_pkt_received_reg[i]
                                <= num_of_host_pkt_received_reg[i] + 1;
                        // else
                        //     num_of_fwd_pkt_received_reg[i]
                        //         <= num_of_fwd_pkt_received_reg[i] + 1;
                    end
                end
            end

            curr_corrupted_cell[i] <= corrupted_cell;

            // --------------------------- Forwarding Logic ------------------------------


            // Put cells to forward in appropriate buffers.
            // TODO: Fix use of valueof and Integer here!
            if (corrupted_cell == 0 && dummy_bit == 0 && dst_ip != host_index[i])
            begin
                
                

                ServerIndex next_hop = fromInteger(valueof(NUM_OF_SERVERS) + 1);
                Phase next_hop_phase = 0;
                Coordinate next_hop_timeslot = 0;

                if (remaining_spraying_hops > 0)
                begin
                    next_hop_phase = (src_mac_phase + 1) % fromInteger(valueof(NUM_OF_PHASES));
                    
                    // Select random hop if there is more than one hop possible.
                    // Bit#(3) spray_slot = 0;                    
                    // if (valueof(PHASE_SIZE) > 1)
                    // begin  
                    //     spray_slot <- rng_hop.get;
                    //     if(verbose) $display("Random hop selected: %d", spray_slot);
                    // end
                    // next_hop = schedule_table[i][next_hop_phase][spray_slot];

                    // Spray short.
                    Bit#(64) min_buffer_len = fromInteger(valueof(FWD_BUFFER_SIZE));
                    for(Integer j = 0; j < valueof(PHASE_SIZE); j = j + 1)
                    begin
                        ServerIndex nbr = schedule_table[i][next_hop_phase][j];
                        Bit#(64) len = fwd_buffer[i][next_hop_phase][j].elements;
                        if(min_buffer_len > len)
                        begin
                            min_buffer_len = len;
                            next_hop = nbr;
                            next_hop_timeslot = fromInteger(j);
                        end
                    end
                    
                    remaining_spraying_hops = remaining_spraying_hops - 1;
                    // TODO: This shouldn't be set here, in Tx.
                    d.payload[458:456] = remaining_spraying_hops;           // TODO: hard-coded index!
                    if (verbose)
                        $display("[SCHED %d] Rx: Fwd cell to spray_hop=%d (min_buf=%d) remaining spraying hops=%d on phase %d", 
                            host_index[i], next_hop, min_buffer_len, remaining_spraying_hops, next_hop_phase);
                end
                else
                begin
                    // Find next phase where my coordinates don't match with the destination.
                    // TODO: Fix hard-code for next phase init.
                    Integer next_phase = 0;
                    if (src_mac_phase == 1) next_phase = 1;
                    if (src_mac_phase == 2) next_phase = 2;

                    for (Integer phase = 0; phase < valueof(NUM_OF_PHASES); phase = phase + 1)
                    begin
                        next_phase = (next_phase + 1) % valueof(NUM_OF_PHASES);
                        Coordinate dst_coord = get_coordinate(dst_ip, next_phase);
                        Coordinate my_coord = get_coordinate(host_index[i], next_phase);
                        // If next hop not yet found, and phase coordinate doesn't match me, only then set.
                        if (next_hop == fromInteger(valueof(NUM_OF_SERVERS) + 1) &&
                            dst_coord != my_coord) 
                        begin
                            next_hop_phase = fromInteger(next_phase);
                            next_hop_timeslot = get_timeslot_with_matching_coordinate(host_index[i], dst_coord, next_phase);
                            next_hop = schedule_table[i][next_hop_phase][next_hop_timeslot];
                            if (verbose)
                            begin
                                let next_hop_coord = get_coordinate(next_hop, next_phase);
                                $display("[SCHED %d] Rx: Fwd cell to direct_hop=%d on phase=%d for dst=%d, SANITY_CHECK=%d", 
                                            host_index[i], next_hop, next_phase, dst_ip, next_hop_coord == dst_coord);
                            end
                        end
                    end
                end
                if (!fwd_buffer[i][next_hop_phase][next_hop_timeslot].full)
                    fwd_buffer[i][next_hop_phase][next_hop_timeslot].write_request.put
                        (makeWriteReq(d.sop, d.eop, d.payload));
                else $display("[SCHED %d] FWD BUFFER FULL!!", host_index[i]);
            end

        endrule
    end

/*------------------------------------------------------------------------------*/

                                /* Shoal */

/*------------------------------------------------------------------------------*/

    // For each src dst pair, rule for choosing cell to send between src-dst.
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        for (Integer j = 0; j < valueof(NUM_OF_PHASES); j = j + 1)
        begin
            for (Integer k = 0; k < valueof(PHASE_SIZE); k = k + 1)
            begin

                // Update flow eligibility for each flow via current neighbor,
                // based on tokens parsed in receive path.
                // rule update_tokens;
                //     let d <- toGet(update_tokens_fifo[i][j]).get;
                //     if(token1 updated)
                //         pieo_queue_id = get_bucket_id(token1.dst_ip, token1.remaining_spraying_hops);
                //         Flow f = pieo.dequeue(pieo_queue_id);
                //         f.predicate = True;
                //         pieo.enqueue(f);
                //     if(token2 updated)
                //         pieo_queue_id = get_bucket_id(token2.dst_ip, token2.remaining_spraying_hops);
                //         Flow f = pieo.dequeue(pieo_queue_id);
                //         f.predicate = True;
                //         pieo.enqueue(f);
                //     choose_buffer_to_send_from_fifo[i][j].enq(?);
                // endrule

                // To select local flow to send, we want to pick the next flow 
                // in round robin that has cells to send.  
                rule choose_buffer_to_send_from;
                    let dc <- toGet(choose_buffer_to_send_from_fifo[i][j][k]).get;

                    // First priority to remote cells to forward.
                    if (!fwd_buffer[i][j][k].empty)
                    begin
                        buffer_to_send_from[i] <= FWD;
                        if (verbose)
                            $display("[SCHED %d] chose to send FWD cell.", host_index[i]);
                    end

                    // Then send any local flows that are ready, else send dummy flows. 
                    else
                    begin                    
                        // ------- pick host flow to send -----------
                        // Initialize dst with next_local_flow_idx[i]
                        // TODO: No easy way to convert from ServerIndex to Integer?
                        // TODO: There has to be a better way to do this!! Ex: PIEO?
                        Integer dst = 0;
                        for(Integer d = 0; d < valueof(NUM_OF_SERVERS); d = d + 1)
                            if (next_local_flow_idx[i] == fromInteger(d))
                                dst = d;
                        
                        ServerIndex host_flow_chosen = fromInteger(valueof(NUM_OF_SERVERS)) + 1;
                        for(Integer d = 0; d < valueof(NUM_OF_SERVERS); d = d + 1)
                        begin
                            if (!cell_generator[i].isEmpty(dst) && dst != i &&
                                host_flow_chosen == fromInteger(valueof(NUM_OF_SERVERS) + 1))
                                host_flow_chosen = fromInteger(dst);
                            dst = (dst + 1) % valueof(NUM_OF_SERVERS);
                        end
                        // ========================================
                        // If we were to use PIEO here:

                        // // does NULL exist in BSV??
                        // Flow f = local_flow_queue.dequeue();
                        // if (f != NULL)
                        // begin 
                        //     f.rank = last rank  // might not need to set this
                        //     // how to set eligibility??
                        //     local_flow_queue.enqueue(f);
                        // end
                        // else dummy cell

                        // ----------------------------------------

                        // At least one host flow was ready to be sent, so will choose HOST.
                        if (host_flow_chosen != fromInteger(valueof(NUM_OF_SERVERS) + 1))
                        begin
                            if (verbose)
                                $display("[SCHED %d] chose to send HOST cell.", host_index[i]);
                            buffer_to_send_from[i] <= HOST;
                            host_flow_to_send[i] <= host_flow_chosen;

                            // Set host flow index for next turn. Check to not send to self.
                            ServerIndex next_flow_idx = (host_flow_chosen + 1) % fromInteger(valueof(NUM_OF_SERVERS));
                            if (next_flow_idx == fromInteger(i))
                                next_local_flow_idx[i] <= (next_flow_idx + 1) % fromInteger(valueof(NUM_OF_SERVERS));
                            else
                                next_local_flow_idx[i] <= next_flow_idx;
                        end

                        else
                        begin
                            if (verbose)
                                $display("[SCHED %d] chose to send dummy cell.", host_index[i]);
                            buffer_to_send_from[i] <= DUMMY;
                        end
                    end

                endrule
            end
        end
    end

/*------------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------*/

                                /* Interface */

/*------------------------------------------------------------------------------*/

// The WAIT_FOR_START_SIG option sets the start flag only on getting a signal from the mac.
// This happens when the Mac receives its first cell.
// TODO: our code might not work with this option, because if start_flag is set on first receive, 
// the receive path is then activated activated. But we need to ensure that coordinates
// and schedule table have been set. May need to add checks for this.
`ifdef WAIT_FOR_START_SIG
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        rule start_scheduler;
            let d <- mac.start_scheduler[i].get;
            start_flag[i] <= 1;
        endrule
    end
`endif

    Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp1;
    Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp4;
    Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp7;
    Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(72))) temp8;
    Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp9;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp2;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp3;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp5;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp6;

    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        temp1[i] = toGet(time_slots_fifo[i]);
        temp4[i] = toGet(received_host_pkt_fifo[i]);
        temp7[i] = toGet(received_wrong_dst_pkt_fifo[i]);
        temp8[i] = toGet(latency_fifo[i]);
        temp9[i] = toGet(buffer_length_fifo[i]);
    //     temp2[i] = toGet(sent_host_pkt_fifo[i]);
    //     temp3[i] = toGet(sent_fwd_pkt_fifo[i]);
    //     temp5[i] = toGet(received_fwd_pkt_fifo[i]);
    //     temp6[i] = toGet(received_corrupted_pkt_fifo[i]);
    end

    // Should prob change name from first_host_index to fpga_idx or altera_idx
    method Action start(ServerIndex first_host_index, Bit#(8) t);
        timeslot_len <= t;
        for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
        begin
            host_index[i] <= (first_host_index
                * fromInteger(valueof(NUM_OF_ALTERA_PORTS))) + fromInteger(i);
`ifndef WAIT_FOR_START_SIG
            start_flag[i] <= 1;
`endif
        end
    endmethod

    method Action stop();
        for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
        begin
            start_flag[i] <= 0;
            running[i] <= 0;
        end
    endmethod

    // Stat Methods
    method Action timeSlotsCount();
        for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
            time_slots_fifo[i].enq(num_of_time_slots_used_reg[i]);
    endmethod

	method Action receivedHostPktCount();
        for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
            received_host_pkt_fifo[i].enq(num_of_host_pkt_received_reg[i]);
	endmethod

	method Action receivedWrongDstPktCount();
        for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
            received_wrong_dst_pkt_fifo[i].enq
                (num_of_wrong_dst_pkt_received_reg[i]);
	endmethod

	method Action latency();
        for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
            latency_fifo[i].enq(latency_reg[i]);
	endmethod

    method Action max_fwd_buffer_length();
        for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
            buffer_length_fifo[i].enq(max_fwd_buffer_length_reg[i]);
    endmethod


	// method Action sentHostPktCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         sent_host_pkt_fifo[i].enq(host_pkt_transmitted_reg[i]);
	// endmethod

	// method Action sentFwdPktCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         sent_fwd_pkt_fifo[i].enq(non_host_pkt_transmitted_reg[i]);
	// endmethod

    // method Action receivedFwdPktCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         received_fwd_pkt_fifo[i].enq(num_of_fwd_pkt_received_reg[i]);
	// endmethod

	// method Action receivedCorruptedPktCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         received_corrupted_pkt_fifo[i].enq
    //             (num_of_corrupted_pkt_received_reg[i]);
	// endmethod

	interface Get time_slots_res = temp1;
	interface Get received_host_pkt_res = temp4;
	interface Get received_wrong_dst_pkt_res = temp7;
	interface Get latency_res = temp8;
    interface Get max_fwd_buffer_length_res = temp9;
    // interface Get sent_host_pkt_res = temp2;
	// interface Get sent_fwd_pkt_res = temp3;
	// interface Get received_fwd_pkt_res = temp5;
	// interface Get received_corrupted_pkt_res = temp6;
endmodule
