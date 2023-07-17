import DefaultValue::*;

`include "ConnectalProjectConfig.bsv"

typedef 512 BUS_WIDTH;
typedef 9 BUS_WIDTH_POW_OF_2;

typedef Bit#(32) Address;

typedef enum {READ, PEEK, WRITE} Operation deriving(Bits, Eq);

typedef struct {
    Bit#(1) sop;
    Bit#(1) eop;
    Bit#(BUS_WIDTH) payload;
} FwdBufferDataT deriving(Bits, Eq);

instance DefaultValue#(FwdBufferDataT);
	defaultValue = FwdBufferDataT {
		sop     : 0,
		eop     : 0,
		payload : 0
	};
endinstance

typedef struct {
    Operation op;
    Bit#(4) bucket_idx;
} ReadReqType deriving(Bits, Eq);

typedef struct {
    FwdBufferDataT data;
} ReadResType deriving(Bits, Eq);

instance DefaultValue#(ReadResType);
    defaultValue = ReadResType {
        data : unpack(0)
    };
endinstance

typedef struct {
    FwdBufferDataT data;
    Bit#(4) bucket_idx;
} WriteReqType deriving(Bits, Eq);

function ReadReqType makeReadReq(Operation op, Bit#(4) bkt);
    return ReadReqType {
        op : op,
        bucket_idx: bkt
    };
endfunction

function ReadResType makeReadRes(FwdBufferDataT data);
    return ReadResType {
        data : data
    };
endfunction

function WriteReqType makeWriteReq
        (Bit#(1) sop, Bit#(1) eop, Bit#(BUS_WIDTH) payload, Bit#(4) bkt);
    FwdBufferDataT d = FwdBufferDataT {
        sop     : sop,
        eop     : eop,
        payload : payload
    };

    return WriteReqType {
        data : d,
        bucket_idx: bkt
    };
endfunction

function ReadReqType makeReadReqCG(Operation op);
    makeReadReq(op, 0);
endfunction

function WriteReqType makeWriteReqCG
        (Bit#(1) sop, Bit#(1) eop, Bit#(BUS_WIDTH) payload);
    makeWriteReq(sop, eop, payload, '1);
endfunction

