import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import Vector::*;
import DefaultValue::*;
import GetPut::*;
import Clocks::*;
import Real::*;

import ShaleUtil::*;
import RingBufferTypes::*;
// import RingBuffer::*;

`include "ConnectalProjectConfig.bsv"


interface CellGenerator;
    interface Put#(void) dummy_cell_req;
    interface Get#(ReadResType) dummy_cell_res;
    interface Vector#(NUM_OF_SERVERS, Put#(void)) host_cell_req;
    interface Vector#(NUM_OF_SERVERS, Get#(ReadResType)) host_cell_res;

    // interface Put#(ServerIndex) host_cell_req;
    // interface Get#(ReadResType) host_cell_res;

    method Bool isEmpty(Integer i);
    method Action start(ServerIndex host_index, Bit#(16) rate);
    method Action stop();
endinterface

module mkCellGenerator#(Integer cell_size) (CellGenerator);

    Bool verbose = False;

    //interface buffers
    FIFOF#(void) dummy_cell_req_fifo <- mkBypassFIFOF;
    FIFOF#(ReadResType) dummy_cell_res_fifo <- mkBypassFIFOF;
    Vector#(NUM_OF_SERVERS, FIFOF#(void))
        host_cell_req_fifo <- replicateM(mkBypassFIFOF);
    Vector#(NUM_OF_SERVERS, FIFOF#(ReadResType))
        host_cell_res_fifo <- replicateM(mkFIFOF);

    //control registers
    Reg#(Bit#(1)) start_flag <- mkReg(0);
    Reg#(ServerIndex) host_index <- mkReg(maxBound);
    Reg#(Bit#(16)) rate_reg <- mkReg(0);
    Reg#(Bit#(16)) num_of_cycles_to_wait <- mkReg(maxBound);
    Vector#(NUM_OF_SERVERS, Reg#(Bit#(2))) 
        num_host_cells_ready <- replicateM(mkReg(0));

    //host cell buffer
    // RingBuffer#(ReadReqType, ReadResType, WriteReqType)
    //     host_buffer <- mkRingBuffer(2, cell_size);

    //dummy cell buffer
    // RingBuffer#(ReadReqType, ReadResType, WriteReqType)
    //     dummy_buffer <- mkRingBuffer(1, cell_size);

/*-------------------------------------------------------------------------------*/
    //put the dummy cell in the buffer
    // Integer max_dummy_block_num = cell_size / valueof(BUS_WIDTH);
    // Reg#(Bit#(16)) curr_dummy_block_num <- mkReg(maxBound);

    // rule put_dummy_cell_in_buffer_initializer
    //     (start_flag == 1
    //     && (curr_dummy_block_num == maxBound
    //         || curr_dummy_block_num < fromInteger(max_dummy_block_num)));

    //     if (curr_dummy_block_num == maxBound)
    //         curr_dummy_block_num <= 0;
    //     else
    //         curr_dummy_block_num <= curr_dummy_block_num + 1;
    // endrule

    // for (Integer i = 0; i < max_dummy_block_num; i = i + 1)
    // begin
    //     rule put_dummy_block_in_buffer (curr_dummy_block_num == fromInteger(i));
    //         Header h = defaultValue;
    //         if (i == 0)
    //         begin
    //             h.dummy_cell_bit = 1;
    //         end
    //         Bit#(HEADER_SIZE) hd = pack(h);
    //         Bit#(BUS_WIDTH) data = {hd, '0};
    //         Bit#(1) sop = 0;
    //         Bit#(1) eop = 0;
    //         if (curr_dummy_block_num == 0)
    //             sop = 1;
    //         if (curr_dummy_block_num == fromInteger(max_dummy_block_num) - 1)
    //             eop = 1;
    //         dummy_buffer.write_request.put(makeWriteReq(sop, eop, data));
    //         if (verbose)
    //             $display("[DMA %d] dummy data = %d %d %x",
    //                 host_index, sop, eop, data);
    //     endrule
    // end

/*-------------------------------------------------------------------------------*/
    //put host cells in respective buffers
    // Integer max_host_block_num = cell_size / valueof(BUS_WIDTH);
    // Reg#(Bit#(16)) curr_host_block_num <- mkReg(maxBound);
    Reg#(Bit#(16)) wait_period <- mkReg(0);
    Reg#(Bit#(1)) prev_cell_put_in_buffer <- mkReg(1);
    Vector#(NUM_OF_SERVERS, Reg#(Bit#(14)))
        curr_seq_num <- replicateM(mkReg(0));

    // Count num_of_cycles_to_wait cycles after last cell.
    rule put_next_host_cell
        (start_flag == 1 && prev_cell_put_in_buffer == 1);

        if (wait_period == num_of_cycles_to_wait)
        begin
            wait_period <= 0;
            // curr_host_block_num <= 0;
            prev_cell_put_in_buffer <= 0;
        end
        else
            wait_period <= wait_period + 1;
    endrule

    for (Integer i = 0; i < valueof(NUM_OF_SERVERS); i = i + 1)
    begin
        rule next_cells_ready(prev_cell_put_in_buffer == 0 
                                && num_host_cells_ready[i] < 3);
            num_host_cells_ready[i] <= num_host_cells_ready[i] + 1;
        endrule
    end

    // for (Integer j = 0; j < max_host_block_num; j = j + 1)
    // begin
    //     rule put_host_block_in_buffer
    //         (curr_host_block_num == fromInteger(j));

    //         Header h = defaultValue;
    //         if (j == 0)
    //         begin
    //             h.src_ip = host_index;
    //             // h.dst_ip = fromInteger(i);
    //             // h.seq_num = curr_seq_num[i];
    //             // curr_seq_num[i] <= curr_seq_num[i] + 1;

    //             // Each local cell has h spray hops in total, including the first hop from the src.
    //             // So, all host cells should have remaining spray hops h-1.
    //             h.remaining_spraying_hops = fromInteger(valueof(NUM_OF_PHASES) - 1);
    //         end
    //         Bit#(HEADER_SIZE) hd = pack(h);
    //         Bit#(BUS_WIDTH) data = {hd, '0};

    //         // Start and end of packet??
    //         Bit#(1) sop = 0;
    //         Bit#(1) eop = 0;
    //         if (curr_host_block_num == 0)
    //             sop = 1;
    //         // Note that curr_host_block_num is not reset here.
    //         // It will be reset by put_next_host_cell, after waiting num_of_cycles_to_wait.
    //         if (curr_host_block_num == fromInteger(max_host_block_num) - 1)
    //         begin
    //             eop = 1;
    //             prev_cell_put_in_buffer <= 1;
    //         end

    //         host_buffer.write_request.put(makeWriteReq(sop, eop, data));

    //         curr_host_block_num <= curr_host_block_num + 1;

    //         // if (verbose && host_index == 0)
    //         //     $display("[DMA %d->%d] seq = %d data = %d %d %x",
    //         //         host_index, i, curr_seq_num[i], sop, eop, data);
    //     endrule
    // end

/*-------------------------------------------------------------------------------*/
    // If rate is in bits/ns, then you wish to send one cell in (cell size/(6.4 * rate))
    // cycles. But it takes only (cell size/bus width)) to send a cell. The diff between
    // these is the cycles you want to wait between consecutive cells.
    // formula = round(((cell size/(6.4 * rate) - (cell size/bus width))) - 1)
    function Integer cycles_to_wait(Integer rate);
        Real x = 6.4 * fromInteger(rate);
        Real y = fromInteger(cell_size)/x;
        Real z = y - (fromInteger(cell_size/valueof(BUS_WIDTH))) - 1;
        return round(z);
    endfunction

	Reg#(Bit#(1)) rate_set_flag <- mkReg(0);
	rule decodeRate (rate_set_flag == 1);
        // It is perhaps more efficient to do cases than a for loop
        // because the compilation might turn this into a lookup table.
        // TODO: Verify.
        Integer rate = 10;          // default rate is 10 bits/ns
        case (rate_reg)
			10      : rate = 10;
			9       : rate = 9;
			8       : rate = 8;
			7       : rate = 7;
			6       : rate = 6;
			5       : rate = 5;
			4       : rate = 4;
			3       : rate = 3;
			2       : rate = 2;
			1       : rate = 1;
			default : rate = 10;
		endcase
        Integer cycles_to_wait_ret = cycles_to_wait(rate);
        // $display("[DMA %d] Cycles to wait is %d", host_index, cycles_to_wait_ret);
        num_of_cycles_to_wait <= fromInteger(cycles_to_wait_ret);
		start_flag <= 1;
		rate_set_flag <= 0;
	endrule

/*-------------------------------------------------------------------------------*/
    rule handle_dummy_cell_req (start_flag == 1);
        let d <- toGet(dummy_cell_req_fifo).get;
    //     dummy_buffer.read_request.put(makeReadReq(PEEK));
    // endrule

    // rule handle_dummy_cell_res;
    //     let d <- dummy_buffer.read_response.get;
        Header h = defaultValue;
    //         if (i == 0)
    //         begin
        h.dummy_cell_bit = 1;
    //         end
        Bit#(HEADER_SIZE) hd = pack(h);
        Bit#(BUS_WIDTH) data = {hd, '0};
        Bit#(1) sop = 0;
        Bit#(1) eop = 0;
    //         if (curr_dummy_block_num == 0)
        sop = 1;
    //         if (curr_dummy_block_num == fromInteger(max_dummy_block_num) - 1)
        eop = 1;
        RingBufferDataT c = RingBufferDataT {
            sop     : sop,
            eop     : eop,
            payload : data
        };
        ReadResType res = ReadResType {
            data : c
        };
        dummy_cell_res_fifo.enq(res);
    endrule

    // Vector#(NUM_OF_SERVERS, )
    // FIFO#(ServerIndex) dst_requested <- mkSizedFIFO(2);
    for (Integer i = 0; i < valueof(NUM_OF_SERVERS); i = i + 1)
    begin
        rule handle_host_cell_req (start_flag == 1);
            let d <- toGet(host_cell_req_fifo[i]).get;
            // host_buffer.read_request.put(makeReadReq(READ));
            // dst_requested.enq(fromInteger(i));
        // endrule
    // end

    // rule handle_host_cell_res;
        // let d <- host_buffer.read_response.get;
        // let dst <- toGet(dst_requested).get;
        
        // if (d.data.sop == 1)
        // begin
            Header h = defaultValue;
    //         if (j == 0)
    //         begin
            h.src_ip = host_index;
            h.dst_ip = fromInteger(i);
            h.seq_num = curr_seq_num[i];
            curr_seq_num[i] <= curr_seq_num[i] + 1;
            if (num_host_cells_ready[i] > 0)
                num_host_cells_ready[i] <= num_host_cells_ready[i] - 1;

            // Each local cell has h spray hops in total, including the first hop from the src.
            // So, all host cells should have remaining spray hops h-1.
            h.remaining_spraying_hops = fromInteger(valueof(NUM_OF_PHASES) - 1);
    //         end
            Bit#(HEADER_SIZE) hd = pack(h);
            Bit#(BUS_WIDTH) data = {hd, '0};

            // Integer s = valueof(BUS_WIDTH)
                // - (valueof(HEADER_SIZE) - valueof(HDR_DST_IP_S));
            // Integer e = s - (valueof(HDR_DST_IP_S) - valueof(HDR_DST_IP_E));
            // $display("[CG %d] Setting [%d:%d] <- %d", host_index, s,e,dst);
            // d.data.payload[s:e] = dst;

            // s = valueof(BUS_WIDTH)
                // - (valueof(HEADER_SIZE) - valueof(HDR_SEQ_NUM_S));
            // e = s - (valueof(HDR_SEQ_NUM_S) - valueof(HDR_SEQ_NUM_E));
            // d.data.payload[s:e] = curr_seq_num[dst];
            // curr_seq_num[dst] <= curr_seq_num[dst] + 1;
        // end
            RingBufferDataT c = RingBufferDataT {
                sop     : 1,
                eop     : 1,
                payload : data
            };
            ReadResType res = ReadResType {
                data : c
            };
            host_cell_res_fifo[i].enq(res);
        endrule
    end

    Vector#(NUM_OF_SERVERS, Put#(void)) temp1;
    Vector#(NUM_OF_SERVERS, Get#(ReadResType)) temp2;

    for (Integer i = 0; i < valueof(NUM_OF_SERVERS); i = i + 1)
    begin
        temp1[i] = toPut(host_cell_req_fifo[i]);
        temp2[i] = toGet(host_cell_res_fifo[i]);
    end

    interface dummy_cell_req = toPut(dummy_cell_req_fifo);
    interface dummy_cell_res = toGet(dummy_cell_res_fifo);
    interface host_cell_req = temp1;
    interface host_cell_res = temp2;
    // interface get_started_signal = toGet(started_signal_fifo);
    // interface host_cell_req = toPut(host_cell_req_fifo);
    // interface host_cell_res = toGet(host_cell_res_fifo);

    method Action start(ServerIndex idx, Bit#(16) rate);
        $display("[DMA (%d)] Starting..........................", idx);
		rate_reg <= rate;
		host_index <= idx; // commenting this is not helping
		rate_set_flag <= 1;
        // started_signal_fifo.enq(?);
    endmethod

    method Action stop();
        $display("[DMA (%d)] Stopping..........................", host_index);
        start_flag <= 0;
    endmethod

    // TODO: Not sure if this is correct and/or efficient?
    method Bool isEmpty(Integer i);
        if ((host_index == fromInteger(i)) || num_host_cells_ready[i] == 0)
            return True;
        else return False;
    endmethod

endmodule
