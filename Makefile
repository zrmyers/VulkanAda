#!/bin/bash

# This Makefile is used to build, install, and update the VulkanAda library, as
# well as to build its tests.
.PHONY: default samples

default: samples

DIRS = obj bin lib

$(DIRS): 
	@echo "Making Directory $@"
	mkdir -p $@

samples: $(DIRS)
	gprbuild -p vulkan-test.gpr
	
clean_samples: 
	gprclean -r -Pvulkan-test.gpr

clean: clean_samples
	rm -rf $(DIRS)
