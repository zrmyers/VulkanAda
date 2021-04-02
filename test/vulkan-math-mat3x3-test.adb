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
with Vulkan.Test.Framework;
with Vulkan.Math.Mat3x3;
with Vulkan.Math.GenFType;
with Vulkan.Math.Vec3;
with Vulkan.Math.Operators;

use Ada.Text_IO;
use Vulkan.Test.Framework;
use Vulkan.Math.Mat3x3;
use Vulkan.Math.GenFType;
use Vulkan.Math.Vec3;
use Vulkan.Math.Operators;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides tests for single precision floating point mat3x3.
--------------------------------------------------------------------------------
package body Vulkan.Math.Mat3x3.Test is

-- Test Mat3x3
procedure Test_Mat3x3 is

    vec1 : Vkm_Vec3 :=
        Make_Vec3(1.0, 2.0, 3.0);

    mat1 : Vkm_Mat3x3 :=
        Make_Mat3x3;

    mat2 : Vkm_Mat3x3 :=
        Make_Mat3x3(diag => 2.0);

    mat3 : Vkm_Mat3x3 :=
        Make_Mat3x3(vec1);

    mat4 : Vkm_Mat3x3 :=
        Make_Mat3x3(0.0, 1.0, 2.0,
                    3.0, 4.0, 5.0,
                    6.0, 7.0, 8.0);

    mat5 : Vkm_Mat3x3 :=
        Make_Mat3x3(vec1, -vec1, vec1 / 2.0);

    mat6 : Vkm_Mat3x3 :=
        Make_Mat3x3(mat4);

begin

    Put_Line("Testing Mat3x3 Constructors...");

    Put_Line("mat1 " & mat1.Image);
    Assert_Mat3x3_Equals(mat1, 1.0, 0.0, 0.0,
                               0.0, 1.0, 0.0,
                               0.0, 0.0, 1.0);

    Put_Line("mat2 " & mat2.Image);
    Assert_Mat3x3_Equals(mat2, 2.0, 0.0, 0.0,
                               0.0, 2.0, 0.0,
                               0.0, 0.0, 2.0);

    Put_Line("mat3 " & mat3.Image);
    Assert_Mat3x3_Equals(mat3, 1.0, 0.0, 0.0,
                               0.0, 2.0, 0.0,
                               0.0, 0.0, 3.0);

    Put_Line("mat4 " & mat4.Image);
    Assert_Mat3x3_Equals(mat4, 0.0, 1.0, 2.0,
                               3.0, 4.0, 5.0,
                               6.0, 7.0, 8.0);

    Put_Line("mat5 " & mat5.Image);
    Assert_Mat3x3_Equals(mat5,  1.0,  2.0,  3.0,
                               -1.0, -2.0, -3.0,
                                0.5,  1.0,  1.5);

    Put_Line("mat6 " & mat6.Image);
    Assert_Mat3x3_Equals(mat6, 0.0, 1.0, 2.0,
                               3.0, 4.0, 5.0,
                               6.0, 7.0, 8.0);

    Put_Line("Testing '=' operator...");
    Put_Line(" mat2 != mat3");
    Assert_Vkm_Bool_Equals(mat2 = mat3, False);

    Put_Line(" mat4 != mat5");
    Assert_Vkm_Bool_Equals(mat4 = mat5, False);

    Put_Line(" mat4 = mat6");
    Assert_Vkm_Bool_Equals(mat4 = mat6, True);

    Put_Line(" Testing unary '+/-' operator");
    Put_Line(" + mat4 = " & Image(+ mat4));
    Assert_Mat3x3_Equals(+mat4, 0.0, 1.0, 2.0,
                                3.0, 4.0, 5.0,
                                6.0, 7.0, 8.0);

    Put_Line(" - mat4 = " & Image(- mat4));
    Assert_Mat3x3_Equals(-mat4, -0.0, -1.0, -2.0,
                                -3.0, -4.0, -5.0,
                                -6.0, -7.0, -8.0);

    Put_Line("+ ( - mat4) = " & Image(+(- mat4)));
    Assert_Mat3x3_Equals(-mat4, -0.0, -1.0, -2.0,
                                -3.0, -4.0, -5.0,
                                -6.0, -7.0, -8.0);

    Put_Line("Testing 'abs' operator...");
    Put_Line(" abs( - mat4) = " & Image(abs(-mat4)));
    Assert_Mat3x3_Equals(abs(-mat4), 0.0, 1.0, 2.0,
                                     3.0, 4.0, 5.0,
                                     6.0, 7.0, 8.0);

    Put_Line("Testing '+' operator...");
    Put_Line(" mat4 + mat5 = " & Image(mat4 + mat5));
    Assert_Mat3x3_Equals(mat4 + mat5, 1.0, 3.0, 5.0,
                                      2.0, 2.0, 2.0,
                                      6.5, 8.0, 9.5);

    Put_Line("Testing '-' operator...");
    Put_Line(" mat4 - mat5 = " & Image(mat4 -mat5));
    Assert_Mat3x3_Equals(mat4 - mat5, -1.0, -1.0, -1.0,
                                       4.0,  6.0,  8.0,
                                       5.5,  6.0,  6.5);

end Test_Mat3x3;

end Vulkan.Math.Mat3x3.Test;
