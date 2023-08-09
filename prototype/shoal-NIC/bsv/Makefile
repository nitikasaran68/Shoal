DEV_ROOT ?= $(HOME)
CONNECTALDIR ?= $(DEV_ROOT)/connectal
PROJECT_DIR = $(DEV_ROOT)/Shoal/prototype/shoal-NIC
PIEO_DIR = $(DEV_ROOT)/PIEO-Scheduler
S2H_INTERFACES = SchedulerTopSimRequest:SchedulerTopSim.request
H2S_INTERFACES = SchedulerTopSim:SchedulerTopSimIndication
BSVFILES = $(PROJECT_DIR)/bsv/SchedulerTopSim.bsv
CPPFILES = $(PROJECT_DIR)/cpp/test_sim.cpp

CONNECTALFLAGS += -D DataBusWidth=128
CONNECTALFLAGS += --bsvpath=$(CONNECTALDIR)/bsv/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../lib/bsv/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../lib/generated/
CONNECTALFLAGS += --bsvpath=$(PROJECT_DIR)/../lib/pieo/

CONNECTALFLAGS += -D NIC_SIM
CONNECTALFLAGS += -D MULTI_NIC
CONNECTALFLAGS += -D SHOAL

CONNECTALFLAGS += --bscflags="-u -show-range-conflict +RTS -K93554432 -RTS -demote-errors G0066:G0045 -suppress-warnings G0046:G0020:S0015:S0080:S0039 -steps-max-intervals 20"

#ifeq ($(BOARD), vsim)
CONNECTALFLAGS += --verilog $(IPDIR)/$(BOARD)/simulation/mac_10gbe
CONNECTALFLAGS += --verilog $(PROJECT_DIR)/../lib/pieo_rtl
#endif

prebuild::
	# (cd $(BOARD); BUILDCACHE_CACHEDIR=$(BUILDCACHE_CACHEDIR) $(BUILDCACHE) quartus_sh -t $(PROJECT_DIR)/../lib/scripts/generate-mac.tcl)

include $(CONNECTALDIR)/Makefile.connectal
