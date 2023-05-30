import Vector::*;
import FIFO::*;
import FIFOF::*;
import ClientServer::*;
import SpecialFIFOs::*;
import BRAM::*;
import GetPut::*;
import DefaultValue::*;
import Clocks::*;

import Params::*;
import SchedulerTypes::*;
import RingBufferTypes::*;
import RingBuffer::*;
import CellGenerator::*;
import Mac::*;

`include "ConnectalProjectConfig.bsv"

interface Scheduler;
    // Responses to stats request?
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     time_slots_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     sent_host_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     sent_fwd_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     received_host_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     received_fwd_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     received_corrupted_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     received_wrong_dst_pkt_res;
	// interface Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64)))
    //     latency_res;

    // // Stats
    // method Action timeSlotsCount();
	// method Action sentHostPktCount();
	// method Action sentFwdPktCount();
	// method Action receivedHostPktCount();
	// method Action receivedFwdPktCount();
	// method Action receivedCorruptedPktCount();
	// method Action receivedWrongDstPktCount();
    // method Action latency();

    method Action start(ServerIndex first_host_index, Bit#(8) t);
    method Action stop();
endinterface

module mkScheduler#(Mac mac, Vector#(NUM_OF_ALTERA_PORTS, CellGenerator) cell_generator,
                Clock pcieClock, Reset pcieReset) (Scheduler);

    Bool verbose = False;

    Clock defaultClock <- exposeCurrentClock();
    Reset defaultReset <- exposeCurrentReset();

    Reg#(Bit#(8)) timeslot_len <- mkReg(0);

    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(1))) start_flag <- replicateM(mkReg(0));

    /*------------------------------------------------------------------------------*/

                                /* Init Path (2 cycles?) */

    /*------------------------------------------------------------------------------*/

    // We need to maintain host index as well as coordinates.
    // Each node has h coordinates, h = NUM_OF_PHASES.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex)) host_index
        <- replicateM(mkReg(maxBound));
    Vector#(NUM_OF_ALTERA_PORTS, Vector#(NUM_OF_PHASES, Reg#(Coordinate)))
        host_coordinates <- replicateM(replicateM(mkReg(0)));

    // For each port, populate coordinates once at module start up.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(1))) coordinates_set <- replicateM(mkReg(0));
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        // TODO: add rule condition that host_index is set correctly.
        rule populate_host_coordinates(coordinates_set[i] == 0);
            Integer n = valueof(host_index[i]);
            // Loop over coordinates in reverse order.
            for (Integer j = NUM_OF_PHASES-1; j >=0; j = j - 1)
            begin      
                host_coordinates[i][j] <= fromInteger(n % (valueof(NODES_PER_PHASE)));
                n = n / (valueof(NODES_PER_PHASE));
            end            
        endrule
    end

    // The schedule is a 3D matrix, indexed by host_idx, phase, slot_within_phase.
	Vector#(NUM_OF_SERVERS, Vector#(NUM_OF_PHASES, Vector#(PHASE_SIZE, Reg#(ServerIndex))))
        schedule_table <- replicateM(replicateM(replicateM(mkReg(0))));

    // Once coordinates set, load schedule into table for each port.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(1))) schedule_initialized <- replicateM(mkReg(0));
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
        begin
        rule populate_schedule_table (schedule_initialized == 0 && coordinates_set[i] = 1);
            schedule_initialized <= 1;
            
            for (Integer phase = 0; phase < valueof(NUM_OF_PHASES); phase = phase + 1)  // Phase
            begin
                for (Integer j = 0; j < valueof(PHASE_SIZE); j = j + 1)                 // Slot within phase
                begin
                    schedule_table[i][phase][j] <= offset_node_in_phase(host_coordinates, phase, j);
                end
            end
        endrule
    end

    // ------------ Helper functions for coordinate math ------------
    function ServerIndex offset_node_in_phase(Vector#(NUM_OF_PHASES, Reg#(Coordinate)) coordinates, Integer phase, Integer offset);
        Integer node = 0;
        Integer k = 1;
        for (Integer coord_idx = valueof(NUM_OF_PHASES) - 1; coord_idx >= 0; coord_idx = coord_idx - 1)
        begin
            Coordinate c = coordinates[coord_idx];
            if (phase == coord_idx)
                c = (c + offset) % valueof(NODES_PER_PHASE);
            node += (valueof(c) * k);
            k = k * NODES_PER_PHASE; 
        end
        return fromInteger(node);
    endfunction

    function ServerIndex get_lowest_node_id_in_phase(Integer node_id, Integer phase);
        Integer lowest = node_id - (node_id % (valueof(NODES_PER_PHASE) ** (phase + 1))) ;   // all coords to the left of phase remain the same, others 0 
        lowest = lowest + (node_id % (valueof(NODES_PER_PHASE) ** phase));                   // all coords to the right of phase remain the same 
        return fromInteger(lowest);
    endfunction

    /* ---------------- Stats ----------------- */
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) time_slots_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) sent_host_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) sent_fwd_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) received_host_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) received_fwd_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) received_corrupted_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) received_wrong_dst_pkt_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));
	// Vector#(NUM_OF_ALTERA_PORTS, SyncFIFOIfc#(Bit#(64))) latency_fifo
	//         <- replicateM(mkSyncFIFO(1, defaultClock, defaultReset, pcieClock));

	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64))) num_of_time_slots_used_reg
    //     <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64))) host_pkt_transmitted_reg
    //     <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64))) non_host_pkt_transmitted_reg
    //     <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
    //     num_of_host_pkt_received_reg <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
    //     num_of_fwd_pkt_received_reg <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
    //     num_of_corrupted_pkt_received_reg <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
    //     num_of_wrong_dst_pkt_received_reg <- replicateM(mkReg(0));
	// Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
    //     latency_reg <- replicateM(mkReg(0));

/*------------------------------------------------------------------------------*/

                                /* Temporal State */

/*------------------------------------------------------------------------------*/

    // Clock for stats.
    Reg#(Bit#(64)) current_time <- mkReg(0);
    rule clk;
        current_time <= current_time + 1;
    endrule

    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(8)))
        clock_within_timeslot <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Coordinate))
        current_timeslot <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Phase))
        current_phase <- replicateM(mkReg(0));
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(64)))
        current_epoch <- replicateM(mkReg(0));

/*------------------------------------------------------------------------------*/

                                /* Tx Path */

/*------------------------------------------------------------------------------*/

    // Tx neighbor.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(ServerIndex))
        current_neighbor <- replicateM(mkReg(maxBound));

    // BRAM to store cells to forward.
    Vector#(NUM_OF_ALTERA_PORTS, Vector#(NUM_OF_SERVERS,
        RingBuffer#(ReadReqType, ReadResType, WriteReqType)))
            fwd_buffer <- replicateM(replicateM
                (mkRingBuffer(valueof(FWD_BUFFER_SIZE), valueof(CELL_SIZE))));
    
    // State to determine next cell to send.
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(BufType))
        buffer_to_send_from <- replicateM(mkReg(HOST));
    Vector#(NUM_OF_ALTERA_PORTS, FIFO#(ReadResType))
        cell_to_send_fifo <- replicateM(mkSizedFIFO(2));
    
    // Header for outgoing cells. 
    Vector#(NUM_OF_ALTERA_PORTS, Reg#(Bit#(HEADER_SIZE)))
        curr_header <- replicateM(mkReg(0));

    // Destination node of the next local flow we will send out, when there's an opportunity.
    Vector#(NUM_OF_ALTERA_PORTS, ServerIndex) next_local_flow_idx;

    // Tx rules for each port.
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
         
        // Set Neighbor (1 cycle).
        // TODO: Shoal does this slightly differently, check.
        rule set_current_timeslot (start_flag[i] == 1);
        
            if (clock_within_timeslot[i] == timeslot_len - 1)        // New timeslot
                begin
                    current_timeslot[i] <= (current_timeslot[i] + 1)
                        % fromInteger(valueof(PHASE_SIZE));
                    if (current_timeslot[i] == 0)                   // New phase.
                        current_phase[i] <= (current_phase[i] + 1) 
                            % fromInteger(valueof(NUM_OF_PHASES));
                    if (current_phase[i] == 0)                      // New epoch.
                        current_epoch[i] <= current_epoch[i] + 1;
                    ServerIndex d = schedule_table[host_index[i]]
                                [current_phase[i]][current_timeslot[i]];
                    current_neighbor[i] <= d;

                    // Choose whether to send local or remote cells in this timeslot.
                    choose_buffer_to_send_from_fifo[i][j].enq(?);
                end
            
                clock_within_timeslot[i] <= 0;
            else
                clock_within_timeslot[i] <= clock_within_timeslot[i] + 1;
        endrule


        // Tx rules for each destination node. At most one req_*_cell rule gets fired per timeslot,
        // depending on buffer_to_send_from type. These rules only get fired on the third clock cycle
        // of each timeslot. It is obvious that these should only fire on a single clock cycle of the timeslot.
        // This is the 3nd cycle because set_current_timeslot and choose_buffer_to_send_from take 1 cycle each to execute.
        // TODO: Check whether bluespec "rules" execute in a single clock cycle? (BSV ref 6.2.2)
        for (Integer j = 0; j < valueof(NUM_OF_SERVERS); j = j + 1)
        begin
            // Get local cells for next local flow to send.
            rule request_local_cell (start_flag[i] == 1 && clock_within_timeslot[i] == 2
                            && buffer_to_send_from[i] == HOST
                            && next_local_flow_idx[i] == fromInteger(j));
                cell_generator[i].host_cell_req[j].put(?);
                next_local_flow_idx = (next_local_flow_idx + 1) % NUM_OF_SERVERS;
            endrule

            rule get_local_cell;
                // TODO: Make sure cell generator gives cells with proper header and spraying hops = h.
                let d <- cell_generator[i].host_cell_res[j].get;
                cell_to_send_fifo[i].enq(d);
            endrule

            // Get cell to send from FWD buffer of current neighbor.
            rule request_fwd_cell (start_flag[i] == 1 && clock_within_timeslot[i] == 2
                            && buffer_to_send_from[i] == FWD
                            && current_neighbor[i] == fromInteger(j));
                fwd_buffer[i][j].read_request.put(makeReadReq(READ));
            endrule

            rule get_fwd_cell;
                let d <- fwd_buffer[i][j].read_response.get;
                cell_to_send_fifo[i].enq(d);
            endrule
        end

        // rule req_dummy_cell (start_flag[i] == 1 && clock_within_timeslot[i] == 2
        //                 && buffer_to_send_from[i] == DUMMY);
        //     cell_generator[i].dummy_cell_req.put(?);
        // endrule

        // rule get_dummy_cell;
        //     let d <- cell_generator[i].dummy_cell_res.get;
        //     cell_to_send_fifo[i].enq(d);
        // endrule

        // Rule to actually send by forwarding cell to lower layers. Only fires once we enq to cell_to_send_fifo.
        rule send_cell (start_flag[i] == 1);
            let d <- toGet(cell_to_send_fifo[i]).get;

            Bit#(HEADER_SIZE) h = curr_header[i];
            if (d.data.sop == 1)
            begin
                Bit#(HEADER_SIZE) x = {host_index[i], current_neighbor[i], '0};
                Integer s = valueof(BUS_WIDTH) - 1;
                Integer e = valueof(BUS_WIDTH) - valueof(HEADER_SIZE);
                h = d.data.payload[s:e] | x;
                curr_header[i] <= h;
            end

            if (d.data.eop == 1)
                d.data.payload = {h, h, h, h, h, h, h, current_time};
            else
                d.data.payload = {h, h, h, h, h, h, h, h};

            // Put to MAC interface.
            mac.mac_tx_write_req[i].put(d.data);

            if (verbose && host_index[i] == 0)
                $display("[SCHED %d] t = %d dst = %d seq = %d sent = %d %d %x",
                    host_index[i], 0, current_neighbor[i], d.data.payload[475:460],
                    d.data.sop, d.data.eop, d.data.payload);
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

    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        rule receive_cell (start_flag[i] == 1);
            // Get from MAC interface
            let d <- mac.mac_rx_read_res[i].get;

            ServerIndex src_mac = curr_src_mac[i];
            ServerIndex dst_mac = curr_dst_mac[i];
            ServerIndex src_ip = curr_src_ip[i];
            ServerIndex dst_ip = curr_dst_ip[i];
            Phase remaining_spraying_hops = curr_remaining_spray_hops[i];
            Bit#(1) dummy_bit = curr_dummy_bit[i];

            Bit#(HEADER_SIZE) hd = curr_rx_header[i];

            Bit#(1) corrupted_cell = curr_corrupted_cell[i];

            // All the indicies assume BUS_WIDTH of 512; change them if you change
            // BUS_WIDTH

            Bit#(16) cell_size_cnt = curr_cell_size[i];

            if (d.sop == 1)
            begin
                corrupted_cell = 0;
                src_mac = d.payload[511:503];
                dst_mac = d.payload[502:494];
                src_ip = d.payload[493:485];
                dst_ip = d.payload[484:476];
                remaining_spraying_hops = d.payload[something]      // TODO: re-define header for Shale.
                dummy_bit = d.payload[448];

                curr_src_mac[i] <= src_mac;
                curr_dst_mac[i] <= dst_mac;
                curr_src_ip[i] <= src_ip;
                curr_dst_ip[i] <= dst_ip;
                curr_dummy_bit[i] <= dummy_bit;

                hd = d.payload[511:448];
                curr_rx_header[i] <= hd;

                // if (dst_mac != host_index[i])
                //     num_of_wrong_dst_pkt_received_reg[i]
                //         <= num_of_wrong_dst_pkt_received_reg[i] + 1;

                cell_size_cnt = fromInteger(valueof(BUS_WIDTH));
                curr_cell_size[i] <= cell_size_cnt;
            end
            else
            begin
                cell_size_cnt = cell_size_cnt + fromInteger(valueof(BUS_WIDTH));
                curr_cell_size[i] <= cell_size_cnt;
            end

            // ------------- Check for corruption + deliver, completely taken from Shoal ------------

            //assumes header size of 64 and BUS_WIDTH of 512
            Bit#(BUS_WIDTH) c = {hd, hd, hd, hd, hd, hd, hd, hd};

            if (corrupted_cell == 0)
            begin
                if (d.eop == 0)
                begin
                    if (d.payload != c)
                    begin
                        corrupted_cell = 1;
                        // num_of_corrupted_pkt_received_reg[i]
                        //     <= num_of_corrupted_pkt_received_reg[i] + 1;
                    end
                end
                else if (d.eop == 1)
                begin
                    if (cell_size_cnt != fromInteger(valueof(CELL_SIZE))
                        || d.payload[511:64] != c[511:64])
                    begin
                        corrupted_cell = 1;
                        // num_of_corrupted_pkt_received_reg[i]
                        //     <= num_of_corrupted_pkt_received_reg[i] + 1;
                    end

                    // Bit#(64) t = d.payload[63:0];
                    // if (t != 0 && latency_reg[i] == 0)
                    //     latency_reg[i] <= current_time - t;

                    if (corrupted_cell == 0 && dummy_bit == 0)
                    begin
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

            // ------------------------------------------------------------------------------


            // Put cells to forward in appropriate buffers.
            if (corrupted_cell == 0 && dummy_bit == 0 && dst_ip != host_index[i])
            begin
                ServerIndex next_hop = fromInteger(valueof(NUM_OF_SERVERS) + 1);
                if (remaining_spraying_hops > 0)
                begin
                    Phase next_phase = (valueof(current_phase) + 1) % fromInteger(NUM_OF_PHASES);
                    // TODO: Use LFSRs to generate random numbers. (?)
                    next_hop = schedule_table[i][next_phase][rand(PHASE_SIZE)]
                    remaining_spraying_hops = remaining_spraying_hops - 1
                    // TODO: In d.payload set spraying hops
                end
                else
                begin
                    Integer next_phase = valueof(current_phase);
                    for (Integer phase = 0; phase < NUM_OF_PHASES; phase = phase + 1)
                    begin
                        next_phase = (valueof(next_phase) + 1) % valueof(NUM_OF_PHASES);
                        if (next_hop == fromInteger(valueof(NUM_OF_SERVERS) + 1) &&
                             get_coordinate(dst_ip, next_phase) != host_coordinates[i][next_phase]) 
                            next_hop = get_node_with_matching_coordinate(dst_ip, next_phase);
                    end
                end
                if (!fwd_buffer[i][next_hop].full)
                    fwd_buffer[i][next_hop].write_request.put
                        (makeWriteReq(d.sop, d.eop, d.payload));
                // else pkt drop. TODO: instead of random, pick buffer with space.
            end

            if (verbose && host_index[i] == 1)
                $display("[SCHED %d] recvd = %d %d %x dst_mac = %d dst_ip = %d",
                    host_index[i], d.sop, d.eop, d.payload, dst_mac, dst_ip);
        endrule
    end

/*------------------------------------------------------------------------------*/

                                /* Shoal */

/*------------------------------------------------------------------------------*/

    Vector#(NUM_OF_ALTERA_PORTS, Vector#(NUM_OF_SERVERS, FIFO#(void)))
        choose_buffer_to_send_from_fifo <- replicateM(replicateM(mkFIFO));

    // For each src dst pair, rule for choosing cell to send between src-dst.
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        for (Integer j = 0; j < valueof(NUM_OF_SERVERS); j = j + 1)
        begin

            rule choose_buffer_to_send_from;
                let d <- toGet(choose_buffer_to_send_from_fifo[i][j]).get;

                // First priority to remote cells to forward.
                if (!fwd_buffer[i][j].empty)
                begin
                    buffer_to_send_from[i] <= FWD;
                end
                // Then send any local flows. 
                // TODO: I don't think there is a requirement for dummy cells here?
                else
                begin
                    buffer_to_send_from[i] <= HOST;
                end

            endrule
        end
    end

/*------------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------*/

                                /* Interface */

/*------------------------------------------------------------------------------*/

`ifdef WAIT_FOR_START_SIG
    for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    begin
        rule start_scheduler;
            let d <- mac.start_scheduler[i].get;
            start_flag[i] <= 1;
        endrule
    end
`endif

    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp1;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp2;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp3;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp4;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp5;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp6;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp7;
    // Vector#(NUM_OF_ALTERA_PORTS, Get#(Bit#(64))) temp8;

    // for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    // begin
    //     temp1[i] = toGet(time_slots_fifo[i]);
    //     temp2[i] = toGet(sent_host_pkt_fifo[i]);
    //     temp3[i] = toGet(sent_fwd_pkt_fifo[i]);
    //     temp4[i] = toGet(received_host_pkt_fifo[i]);
    //     temp5[i] = toGet(received_fwd_pkt_fifo[i]);
    //     temp6[i] = toGet(received_corrupted_pkt_fifo[i]);
    //     temp7[i] = toGet(received_wrong_dst_pkt_fifo[i]);
    //     temp8[i] = toGet(latency_fifo[i]);
    // end

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
            start_flag[i] <= 0;
    endmethod

    // Stat Methods
    // method Action timeSlotsCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         time_slots_fifo[i].enq(num_of_time_slots_used_reg[i]);
    // endmethod

	// method Action sentHostPktCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         sent_host_pkt_fifo[i].enq(host_pkt_transmitted_reg[i]);
	// endmethod

	// method Action sentFwdPktCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         sent_fwd_pkt_fifo[i].enq(non_host_pkt_transmitted_reg[i]);
	// endmethod

	// method Action receivedHostPktCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         received_host_pkt_fifo[i].enq(num_of_host_pkt_received_reg[i]);
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

	// method Action receivedWrongDstPktCount();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         received_wrong_dst_pkt_fifo[i].enq
    //             (num_of_wrong_dst_pkt_received_reg[i]);
	// endmethod

	// method Action latency();
    //     for (Integer i = 0; i < valueof(NUM_OF_ALTERA_PORTS); i = i + 1)
    //         latency_fifo[i].enq(latency_reg[i]);
	// endmethod

	// interface Get time_slots_res = temp1;
	// interface Get sent_host_pkt_res = temp2;
	// interface Get sent_fwd_pkt_res = temp3;
	// interface Get received_host_pkt_res = temp4;
	// interface Get received_fwd_pkt_res = temp5;
	// interface Get received_corrupted_pkt_res = temp6;
	// interface Get received_wrong_dst_pkt_res = temp7;
	// interface Get latency_res = temp8;
endmodule
