DEV_ROOT ?= $(HOME)
CONNECTALDIR ?= $(DEV_ROOT)/connectal
PROJECT_DIR = $(DEV_ROOT)/Shoal/prototype/shoal-hw-simulator/1-fpga-multiple-nics/
S2H_INTERFACES = ShoalMultiSimTopRequest:ShoalMultiSimTop.request
H2S_INTERFACES = ShoalMultiSimTop:ShoalMultiSimTopIndication
BSVFILES = $(PROJECT_DIR)/bsv/ShoalMultiSimTop.bsv
CPPFILES = $(PROJECT_DIR)/cpp/test_sim.cpp

CONNECTALFLAGS += -D DataBusWidth=128
CONNECTALFLAGS += --bsvpath=$(CONNECTALDIR)/bsv/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../../circuit-switch/bsv/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../../shoal-NIC/bsv/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../../lib/bsv/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../../lib/generated/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../../lib/pieo/

# This flag enables PhySwitch instead of MacSwitch
# CONNECTALFLAGS += -D CW_PHY_SIM

CONNECTALFLAGS += -D NIC_SIM
CONNECTALFLAGS += -D MULTI_NIC
#CONNECTALFLAGS += -D SHOAL
CONNECTALFLAGS += -D WAIT_FOR_START_SIG
CONNECTALFLAGS += -D DEBUG_WITHOUT_BUFFERS

CONNECTALFLAGS += --bscflags="-show-schedule -u -show-range-conflict +RTS -K93554432 -RTS -demote-errors G0066:G0045 -suppress-warnings G0046:G0020:S0015:S0080:S0039 -steps-max-intervals 20"

#ifeq ($(BOARD), vsim)
CONNECTALFLAGS += --verilog $(IPDIR)/$(BOARD)/simulation/mac_10gbe
CONNECTALFLAGS += --verilog $(IPDIR)/$(BOARD)/simulation/altera_xcvr_10gbaser_wrapper
CONNECTALFLAGS += --verilog $(PROJECT_DIR)/../../lib/pieo_rtl

#endif

prebuild::
	(cd $(BOARD); BUILDCACHE_CACHEDIR=$(BUILDCACHE_CACHEDIR) $(BUILDCACHE) quartus_sh -t $(PROJECT_DIR)/../../lib/scripts/generate-mac.tcl)

include $(CONNECTALDIR)/Makefile.connectal
