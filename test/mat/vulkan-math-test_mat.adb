--------------------------------------------------------------------------------
-- MIT License
--
-- Copyright (c) 2021 Zane Myers
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--------------------------------------------------------------------------------
with Vulkan.Math.Mat2x2.Test;
with Vulkan.Math.Mat2x3.Test;
with Vulkan.Math.Mat2x4.Test;
with Vulkan.Math.Mat3x2.Test;
with Vulkan.Math.Mat3x3.Test;
with Vulkan.Math.Mat3x4.Test;
with Vulkan.Math.Mat4x2.Test;
with Vulkan.Math.Mat4x3.Test;
with Vulkan.Math.Mat4x4.Test;

use Vulkan.Math.Mat2x2.Test;
use Vulkan.Math.Mat2x3.Test;
use Vulkan.Math.Mat2x4.Test;
use Vulkan.Math.Mat3x2.Test;
use Vulkan.Math.Mat3x3.Test;
use Vulkan.Math.Mat3x4.Test;
use Vulkan.Math.Mat4x2.Test;
use Vulkan.Math.Mat4x3.Test;
use Vulkan.Math.Mat4x4.Test;

package body Vulkan.Math.Test_Mat is

-- Test Harness for single precision floating point matrices.
procedure Test_Mat is
begin

    Test_Mat2x2;

    Test_Mat2x3;

    Test_Mat2x4;

    Test_Mat3x2;

    Test_Mat3x3;

    Test_Mat3x4;

    Test_Mat4x2;

    Test_Mat4x3;

    Test_Mat4x4;

end Test_Mat;

end Vulkan.Math.Test_Mat;
