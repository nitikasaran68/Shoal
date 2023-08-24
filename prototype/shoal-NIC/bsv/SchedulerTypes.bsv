import ShaleUtil::*;
import DefaultValue::*;

`include "ConnectalProjectConfig.bsv"

typedef enum {HOST, SPRAY, FWD, DUMMY} BufType deriving(Bits, Eq);

