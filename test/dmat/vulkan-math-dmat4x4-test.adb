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
with Vulkan.Math.GenDMatrix;
with Vulkan.Math.Dmat4x4;
with Vulkan.Math.Dmat2x4;
with Vulkan.Math.Dmat4x2;
with Vulkan.Math.GenDType;
with Vulkan.Math.Dvec4;
with Vulkan.Math.Operators;

use Ada.Text_IO;
use Vulkan.Test.Framework;
use Vulkan.Math.Dmat4x4;
use Vulkan.Math.Dmat2x4;
use Vulkan.Math.Dmat4x2;
use Vulkan.Math.GenDType;
use Vulkan.Math.Dvec4;
use Vulkan.Math.Operators;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides tests for single precision floating point mat4x4.
--------------------------------------------------------------------------------
package body Vulkan.Math.Dmat4x4.Test is

-- Test Mat4x4
procedure Test_Dmat4x4 is

    vec1 : Vkm_Dvec4 :=
        Make_Dvec4(1.0, 2.0, 3.0, 4.0);

    mat1 : Vkm_Dmat4x4 :=
        Make_Dmat4x4;

    mat2 : Vkm_Dmat4x4 :=
        Make_Dmat4x4(diag => 2.0);

    mat3 : Vkm_Dmat4x4 :=
        Make_Dmat4x4(vec1);

    mat4 : Vkm_Dmat4x4 :=
        Make_Dmat4x4(0.0, 1.0, 2.0, 3.0,
                    4.0, 5.0, 6.0, 7.0,
                    8.0, 9.0, 10.0, 11.0,
                    12.0, 13.0, 14.0, 15.0);

    mat5 : Vkm_Dmat4x4 :=
        Make_Dmat4x4(vec1, -vec1, vec1 / 2.0, -vec1 / 2.0);

    mat6 : Vkm_Dmat4x4 :=
        Make_Dmat4x4(mat4);

    mat7 : Vkm_Dmat2x4 :=
        Make_Dmat2x4(1.0, 2.0,
                    3.0, 4.0,
                    5.0, 6.0,
                    7.0, 8.0);

    mat8 : Vkm_Dmat4x2 :=
        Make_Dmat4x2(1.0, 2.0, 3.0, 4.0,
                    5.0, 6.0, 7.0, 8.0);

begin

    Put_Line("Testing Mat4x4 Constructors...");

    Put_Line("mat1 " & mat1.Image);
    Assert_Dmat4x4_Equals(mat1, 1.0, 0.0, 0.0, 0.0,
                               0.0, 1.0, 0.0, 0.0,
                               0.0, 0.0, 1.0, 0.0,
                               0.0, 0.0, 0.0, 1.0);

    Put_Line("mat2 " & mat2.Image);
    Assert_Dmat4x4_Equals(mat2, 2.0, 0.0, 0.0, 0.0,
                               0.0, 2.0, 0.0, 0.0,
                               0.0, 0.0, 2.0, 0.0,
                               0.0, 0.0, 0.0, 2.0);

    Put_Line("mat3 " & mat3.Image);
    Assert_Dmat4x4_Equals(mat3, 1.0, 0.0, 0.0, 0.0,
                               0.0, 2.0, 0.0, 0.0,
                               0.0, 0.0, 3.0, 0.0,
                               0.0, 0.0, 0.0, 4.0);

    Put_Line("mat4 " & mat4.Image);
    Assert_Dmat4x4_Equals(mat4,  0.0,  1.0,  2.0,  3.0,
                                4.0,  5.0,  6.0,  7.0,
                                8.0,  9.0, 10.0, 11.0,
                               12.0, 13.0, 14.0, 15.0);

    Put_Line("mat5 " & mat5.Image);
    Assert_Dmat4x4_Equals(mat5,  1.0,  2.0,  3.0,  4.0,
                               -1.0, -2.0, -3.0, -4.0,
                                0.5,  1.0,  1.5,  2.0,
                               -0.5, -1.0, -1.5, -2.0);

    Put_Line("mat6 " & mat6.Image);
    Assert_Dmat4x4_Equals(mat6,  0.0,  1.0,  2.0,  3.0,
                                4.0,  5.0,  6.0,  7.0,
                                8.0,  9.0, 10.0, 11.0,
                               12.0, 13.0, 14.0, 15.0);

    Put_Line("Testing '=' operator...");
    Put_Line(" mat2 != mat3");
    Assert_Vkm_Bool_Equals(mat2 = mat3, False);

    Put_Line(" mat4 != mat5");
    Assert_Vkm_Bool_Equals(mat4 = mat5, False);

    Put_Line(" mat4 = mat6");
    Assert_Vkm_Bool_Equals(mat4 = mat6, True);

    Put_Line(" Testing unary '+/-' operator");
    Put_Line(" + mat4 = " & Image(+ mat4));
    Assert_Dmat4x4_Equals(+mat4, 0.0,  1.0,  2.0,  3.0,
                                4.0,  5.0,  6.0,  7.0,
                                8.0,  9.0, 10.0, 11.0,
                               12.0, 13.0, 14.0, 15.0);

    Put_Line(" - mat4 = " & Image(- mat4));
    Assert_Dmat4x4_Equals(-mat4, -0.0,  -1.0,  -2.0,  -3.0,
                                -4.0,  -5.0,  -6.0,  -7.0,
                                -8.0,  -9.0, -10.0, -11.0,
                               -12.0, -13.0, -14.0, -15.0);

    Put_Line("+ ( - mat4) = " & Image(+(- mat4)));
    Assert_Dmat4x4_Equals(+(-mat4), -0.0,  -1.0,  -2.0,  -3.0,
                                   -4.0,  -5.0,  -6.0,  -7.0,
                                   -8.0,  -9.0, -10.0, -11.0,
                                  -12.0, -13.0, -14.0, -15.0);

    Put_Line("Testing 'abs' operator...");
    Put_Line(" abs( - mat4) = " & Image(abs(-mat4)));
    Assert_Dmat4x4_Equals(abs(-mat4), 0.0,  1.0,  2.0,  3.0,
                                     4.0,  5.0,  6.0,  7.0,
                                     8.0,  9.0, 10.0, 11.0,
                                    12.0, 13.0, 14.0, 15.0);

    Put_Line("Testing '+' operator...");
    Put_Line(" mat4 + mat5 = " & Image(mat4 + mat5));
    Assert_Dmat4x4_Equals(mat4 + mat5, 1.0, 3.0, 5.0, 7.0,
                                      3.0, 3.0, 3.0, 3.0,
                                      8.5, 10.0, 11.5, 13.0,
                                      11.5, 12.0, 12.5, 13.0);

    Put_Line("Testing '-' operator...");
    Put_Line(" mat4 - mat5 = " & Image(mat4 -mat5));
    Assert_Dmat4x4_Equals(mat4 - mat5, -1.0, -1.0, -1.0, -1.0,
                                       5.0,  7.0,  9.0, 11.0,
                                       7.5,  8.0,  8.5,  9.0,
                                      12.5, 14.0,  15.5,  17.0);

    Put_Line("Testing '*' operator...");
    Put_Line(" mat4 * mat8 = " & Image(mat4 * mat8));
    Assert_Dmat4x2_Equals(mat4 * mat8, 34.0, 40.0,
                                      98.0, 120.0,
                                      162.0, 200.0,
                                      226.0, 280.0);

    Put_Line(" mat7 * mat4 = " & Image(mat7 * mat4));
    Assert_Dmat2x4_Equals(mat7*mat4, 80.0, 90.0,
                                    100.0, 110.0,
                                    176.0, 202.0,
                                    228.0, 254.0);

    Put_Line(" vec1 * mat4 = " & Image(vec1 * mat4));
    Assert_Dvec4_Equals(vec1 * mat4, 80.0, 90.0, 100.0, 110.0);

    Put_Line(" mat4 * vec1 = " & Image(mat4 * vec1));
    Assert_Dvec4_Equals(mat4 * vec1, 20.0, 60.0, 100.0, 140.0);

end Test_Dmat4x4;

end Vulkan.Math.Dmat4x4.Test;
