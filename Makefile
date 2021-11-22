################################################################################
## MIT License
##
## Copyright (c) 2020 Zane Myers
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.
################################################################################
# This Makefile is used to build, install, and update the VulkanAda library, as
# well as to build its tests.
#
# For reference, gprbuild documentation can be found here:
#     https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/building_with_gprbuild.html
################################################################################
.PHONY: default samples

default: samples vulkan

DIRS = obj bin lib

$(DIRS):
	@echo "Making Directory $@"
	mkdir $@

vulkan: $(DIRS)
	@echo "Building VulkanAda Library"
	gprbuild -p vulkan.gpr -j0

samples: $(DIRS) vulkan
	gprbuild -p vulkan-test.gpr -j0
	cp -r dependencies/windows/*.dll ./bin

test: ./bin/vulkan_test-math.exe
	./bin/vulkan_test-math.exe

docs: vulkan
	gnatdoc -Pvulkan.gpr -l -w

metrics:
	gnatmetric -Pvulkan.gpr

clean:
	rm -rf $(DIRS)
	rm -rf docs
