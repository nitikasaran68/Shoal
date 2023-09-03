DEV_ROOT ?= $(HOME)
CONNECTALDIR ?= $(DEV_ROOT)/connectal
PROJECT_DIR = $(DEV_ROOT)/Shoal/prototype/circuit-switch
S2H_INTERFACES = CircuitSwTopSimRequest:CircuitSwTopSim.request
H2S_INTERFACES = CircuitSwTopSim:CircuitSwTopSimIndication
BSVFILES = $(PROJECT_DIR)/bsv/CircuitSwTopSim.bsv
CPPFILES = $(PROJECT_DIR)/cpp/test_sim.cpp

CONNECTALFLAGS += -D DataBusWidth=128
CONNECTALFLAGS += --bsvpath=$(CONNECTALDIR)/bsv/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../lib/bsv/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../lib/generated/

# CONNECTALFLAGS += -D CW_SIM
CONNECTALFLAGS += -D MAC_SW
#CONNECTALFLAGS += -D PHY_SW
CONNECTALFLAGS += -D WAIT_FOR_START_SIG

CONNECTALFLAGS += --bscflags="-u -show-range-conflict +RTS -K187108864 -RTS -demote-errors G0066:G0045 -suppress-warnings G0046:G0020:S0015:S0080:S0039 -steps-max-intervals 20"

#ifeq ($(BOARD), vsim)
CONNECTALFLAGS += --verilog $(IPDIR)/$(BOARD)/simulation/mac_10gbe
CONNECTALFLAGS += --verilog $(IPDIR)/$(BOARD)/simulation/altera_xcvr_10gbaser_wrapper
#endif

prebuild::
	# (cd $(BOARD); BUILDCACHE_CACHEDIR=$(BUILDCACHE_CACHEDIR) $(BUILDCACHE) quartus_sh -t $(PROJECT_DIR)/../lib/scripts/connectal-synth-pll.tcl)
	# (cd $(BOARD); BUILDCACHE_CACHEDIR=$(BUILDCACHE_CACHEDIR) $(BUILDCACHE) quartus_sh -t $(PROJECT_DIR)/../lib/scripts/connectal-synth-pma.tcl)
	# (cd $(BOARD); BUILDCACHE_CACHEDIR=$(BUILDCACHE_CACHEDIR) $(BUILDCACHE) quartus_sh -t $(PROJECT_DIR)/../lib/scripts/connectal-synth-misc.tcl)
	(cd $(BOARD); BUILDCACHE_CACHEDIR=$(BUILDCACHE_CACHEDIR) $(BUILDCACHE) quartus_sh -t $(PROJECT_DIR)/../lib/scripts/connectal-synth-phy.tcl)
	(cd $(BOARD); BUILDCACHE_CACHEDIR=$(BUILDCACHE_CACHEDIR) $(BUILDCACHE) quartus_sh -t $(PROJECT_DIR)/../lib/scripts/generate-mac.tcl)
include $(CONNECTALDIR)/Makefile.connectal
