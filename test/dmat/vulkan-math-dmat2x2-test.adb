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
with Ada.Text_IO;
with Vulkan.Math.GenFMatrix;
with Vulkan.Math.Dmat2x2;
with Vulkan.Math.GenDType;
with Vulkan.Math.Dvec2;
with Vulkan.Test.Framework;

use Ada.Text_IO;
use Vulkan.Math.GenFMatrix;
use Vulkan.Math.Dmat2x2;
use Vulkan.Math.GenDType;
use Vulkan.Math.Dvec2;
use Vulkan.Test.Framework;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides tests for single precision floating point mat2x2.
--------------------------------------------------------------------------------
package body Vulkan.Math.Dmat2x2.Test is

-- Test Dmat2x2
procedure Test_Dmat2x2 is

    vec1 : Vkm_Dvec2 :=
        Make_Dvec2(1.0, 2.0);

    mat1 : Vkm_Dmat2x2 :=
        Make_Dmat2x2;

    mat2 : Vkm_Dmat2x2 :=
        Make_Dmat2x2(diag => 2.0);

    mat3 : Vkm_Dmat2x2 :=
        Make_Dmat2x2(vec1);

    mat4 : Vkm_Dmat2x2 :=
        Make_Dmat2x2(0.0, 1.0, 2.0, 3.0);

    mat5 : Vkm_Dmat2x2 :=
        Make_Dmat2x2(vec1, vec1);

    mat6 : Vkm_Dmat2x2 :=
        Make_Dmat2x2(mat4);

begin

    Put_Line("Testing Dmat2x2 Constructors...");

    Put_Line("mat1 " & mat1.Image);
    Assert_Dmat2x2_Equals(mat1, 1.0, 0.0, 0.0, 1.0);

    Put_Line("mat2 " & mat2.Image);
    Assert_Dmat2x2_Equals(mat2, 2.0, 0.0, 0.0, 2.0);

    Put_Line("mat3 " & mat3.Image);
    Assert_Dmat2x2_Equals(mat3, 1.0, 0.0, 0.0, 2.0);

    Put_Line("mat4 " & mat4.Image);
    Assert_Dmat2x2_Equals(mat4, 0.0, 1.0, 2.0, 3.0);

    Put_Line("mat5 " & mat5.Image);
    Assert_Dmat2x2_Equals(mat5, 1.0, 2.0, 1.0, 2.0);

    Put_Line("mat6 " & mat6.Image);
    Assert_Dmat2x2_Equals(mat6, 0.0, 1.0, 2.0, 3.0);

    Put_Line("Testing '=' operator...");
    Put_Line(" mat2 != mat3");
    Assert_Vkm_Bool_Equals(mat2 = mat3, False);

    Put_Line(" mat4 != mat5");
    Assert_Vkm_Bool_Equals(mat4 = mat5, False);

    Put_Line(" mat4 = mat6");
    Assert_Vkm_Bool_Equals(mat4 = mat6, True);

    Put_Line(" Testing unary '+/-' operator");
    Put_Line(" + mat4 = " & Image(+ mat4));
    Assert_Dmat2x2_Equals(+mat4, 0.0, 1.0, 2.0, 3.0);

    Put_Line(" - mat4 = " & Image(- mat4));
    Assert_Dmat2x2_Equals(-mat4, -0.0, -1.0, -2.0, -3.0);

    Put_Line("+ ( - mat4) = " & Image(+(- mat4)));
    Assert_Dmat2x2_Equals(-mat4, -0.0, -1.0, -2.0, -3.0);

    Put_Line("Testing 'abs' operator...");
    Put_Line(" abs( - mat4) = " & Image(abs(-mat4)));
    Assert_Dmat2x2_Equals(abs(-mat4), 0.0, 1.0, 2.0, 3.0);

    Put_Line("Testing '+' operator...");
    Put_Line(" mat4 + mat5 = " & Image(mat4 + mat5));
    Assert_Dmat2x2_Equals(mat4 + mat5, 1.0, 3.0, 3.0, 5.0);

    Put_Line("Testing '-' operator...");
    Put_Line(" mat4 - mat5 = " & Image(mat4 -mat5));
    Assert_Dmat2x2_Equals(mat4 - mat5, -1.0, -1.0, 1.0, 1.0);

    Put_Line("Testing '*' operator...");
    Put_Line(" mat4 * mat5 = " & Image(mat4 * mat5));
    Assert_Dmat2x2_Equals(mat4 * mat5, 1.0, 2.0, 5.0, 10.0);

    Put_Line(" mat4 * vec1 = " & Image(mat4 * vec1));
    Assert_Dvec2_Equals(mat4 * vec1, 2.0, 8.0);

    Put_Line(" vec1 * mat4 = " & Image(vec1 * mat4));
    Assert_Dvec2_Equals(vec1 * mat4, 4.0, 7.0);


end Test_Dmat2x2;

end Vulkan.Math.Dmat2x2.Test;
