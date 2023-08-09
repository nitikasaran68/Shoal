import ShaleUtil::*;
import DefaultValue::*;

`include "ConnectalProjectConfig.bsv"

typedef enum {HOST, FWD, DUMMY} BufType deriving(Bits, Eq);

