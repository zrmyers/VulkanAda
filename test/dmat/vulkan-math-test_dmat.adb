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
with Vulkan.Math.Dmat2x2.Test;
with Vulkan.Math.Dmat2x3.Test;
with Vulkan.Math.Dmat2x4.Test;
with Vulkan.Math.Dmat3x2.Test;
with Vulkan.Math.Dmat3x3.Test;
with Vulkan.Math.Dmat3x4.Test;
with Vulkan.Math.Dmat4x2.Test;
with Vulkan.Math.Dmat4x3.Test;
with Vulkan.Math.Dmat4x4.Test;

use Vulkan.Math.Dmat2x2.Test;
use Vulkan.Math.Dmat2x3.Test;
use Vulkan.Math.Dmat2x4.Test;
use Vulkan.Math.Dmat3x2.Test;
use Vulkan.Math.Dmat3x3.Test;
use Vulkan.Math.Dmat3x4.Test;
use Vulkan.Math.Dmat4x2.Test;
use Vulkan.Math.Dmat4x3.Test;
use Vulkan.Math.Dmat4x4.Test;

package body Vulkan.Math.Test_Dmat is

-- Test Harness for double precision floating point matrices.
procedure Test_Dmat is
begin

    Test_Dmat2x2;

    Test_Dmat2x3;

    Test_Dmat2x4;

    Test_Dmat3x2;

    Test_Dmat3x3;

    Test_Dmat3x4;

    Test_Dmat4x2;

    Test_Dmat4x3;

    Test_Dmat4x4;

end Test_Dmat;

end Vulkan.Math.Test_Dmat;
