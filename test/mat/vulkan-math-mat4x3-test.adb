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
with Ada.Characters.Latin_1;
with Vulkan.Test.Framework;
with Vulkan.Math.GenFMatrix;
with Vulkan.Math.Mat4x3;
with Vulkan.Math.Mat2x2;
with Vulkan.Math.Mat2x4;
with Vulkan.Math.GenFType;
with Vulkan.Math.Vec4;
with Vulkan.Math.Vec3;

use Ada.Text_IO;
use Ada.Characters.Latin_1;
use Vulkan.Math.Mat2x2;
use Vulkan.Math.Mat4x3;
use Vulkan.Math.Mat2x4;
use Vulkan.Math.GenFType;
use Vulkan.Math.Vec4;
use Vulkan.Math.Vec3;
use Vulkan.Test.Framework;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides tests for single precision floating point mat4x3.
--------------------------------------------------------------------------------
package body Vulkan.Math.Mat4x3.Test is

-- Test Mat4x3
procedure Test_Mat4x3 is

    vec1 : Vkm_Vec4 :=
        Make_Vec4(1.0, 2.0, 3.0, 4.0);

    vec2 : Vkm_Vec3 :=
        Make_Vec3(1.0, 2.0, 3.0);

    mat1 : Vkm_Mat4x3 :=
        Make_Mat4x3;

    mat2 : Vkm_Mat4x3 :=
        Make_Mat4x3(0.0, 1.0, 2.0,
                    3.0, 4.0, 5.0,
                    6.0, 7.0, 8.0,
                    9.0, 10.0, 11.0);

    mat3 : Vkm_Mat4x3 :=
        Make_Mat4x3(vec2, - vec2, 2.0 * vec2, -2.0 * vec2);

    mat4 : Vkm_Mat4x3 :=
        Make_Mat4x3(mat2);

    mat5 : Vkm_Mat2x2 :=
        Make_Mat2x2(5.0);

    mat6 : Vkm_Mat4x3 :=
        Make_Mat4x3(mat5);

    mat7 : Vkm_Mat2x4 :=
        Make_Mat2x4( 1.0, -1.0, 0.5, 1.5,
                    -1.0,  2.0, 1.0, 1.0);
begin

    Put_Line(LF & "Testing Mat4x3 Constructors...");

    Put_Line("mat1 " & mat1.Image);
    Assert_Mat4x3_Equals(mat1, 0.0, 0.0, 0.0,
                               0.0, 0.0, 0.0,
                               0.0, 0.0, 0.0,
                               0.0, 0.0, 0.0);

    Put_Line("mat2 " & mat2.Image);
    Assert_Mat4x3_Equals(mat2, 0.0, 1.0, 2.0,
                               3.0, 4.0, 5.0,
                               6.0, 7.0, 8.0,
                               9.0, 10.0, 11.0);

    Put_Line("mat3 " & mat3.Image);
    Assert_Mat4x3_Equals(mat3, 1.0,  2.0,  3.0,
                              -1.0, -2.0, -3.0,
                               2.0,  4.0,  6.0,
                              -2.0, -4.0, -6.0);

    Put_Line("mat4 " & mat4.Image);
    Assert_Mat4x3_Equals(mat4, 0.0, 1.0, 2.0,
                               3.0, 4.0, 5.0,
                               6.0, 7.0, 8.0,
                               9.0, 10.0, 11.0);

    Put_Line("mat6 " & mat6.Image);
    Assert_Mat4x3_Equals(mat6, 5.0, 0.0, 0.0,
                               0.0, 5.0, 0.0,
                               0.0, 0.0, 0.0,
                               0.0, 0.0, 0.0);

    Put_Line("Testing '=' operator...");
    Put_Line(" mat2 != mat3");
    Assert_Vkm_Bool_Equals(mat2 = mat3, False);

    Put_Line(" mat4 != mat5");
    Assert_Vkm_Bool_Equals(mat4 = mat5, False);

    Put_Line(" mat4 = mat2");
    Assert_Vkm_Bool_Equals(mat4 = mat2, True);

    Put_Line(" Testing unary '+/-' operator");
    Put_Line(" + mat4 = " & Image(+ mat4));
    Assert_Mat4x3_Equals(+mat4, 0.0, 1.0, 2.0,
                                3.0, 4.0, 5.0,
                                6.0, 7.0, 8.0,
                                9.0, 10.0, 11.0);

    Put_Line(" - mat4 = " & Image(- mat4));
    Assert_Mat4x3_Equals(-mat4, 0.0,  -1.0, -2.0,
                               -3.0,  -4.0, -5.0,
                               -6.0,  -7.0, -8.0,
                               -9.0, -10.0, -11.0);

    Put_Line("+(- mat4) = " & Image(+(- mat4)));
    Assert_Mat4x3_Equals(+(-mat4), 0.0,  -1.0, -2.0,
                                  -3.0,  -4.0, -5.0,
                                  -6.0,  -7.0, -8.0,
                                  -9.0, -10.0, -11.0);

    Put_Line("Testing 'abs' operator...");
    Put_Line(" abs(- mat4) = " & Image(abs(-mat4)));
    Assert_Mat4x3_Equals(abs(-mat4), 0.0, 1.0, 2.0,
                                     3.0, 4.0, 5.0,
                                     6.0, 7.0, 8.0,
                                     9.0, 10.0, 11.0);

    Put_Line("Testing '+' operator...");
    Put_Line(" mat4 + mat3 = " & Image(mat4 + mat3));
    Assert_Mat4x3_Equals(mat4 + mat3, 1.0, 3.0, 5.0,
                                      2.0, 2.0, 2.0,
                                      8.0, 11.0, 14.0,
                                      7.0, 6.0, 5.0);

    Put_Line("Testing '-' operator...");
    Put_Line(" mat4 - mat3 = " & Image(mat4 -mat3));
    Assert_Mat4x3_Equals(mat4 - mat3, -1.0, -1.0, -1.0,
                                       4.0,  6.0,  8.0,
                                       4.0,  3.0,  2.0,
                                       11.0,  14.0,  17.0);

    Put_Line("Testing '*' operator...");
    Put_Line(" mat7 * mat4 = " & Image(mat7 * mat4));
    Assert_Mat2x3_Equals(mat7 * mat4, 13.5,  15.5,  17.5,
                                      21.0,  24.0,  27.0);

    Put_Line(" mat4 * vec2 = " & Image(mat4 * vec2));
    Assert_Vec4_Equals(mat4 * vec2, 8.0, 26.0, 44.0, 62.0);

    Put_Line(" vec1 * mat4 = " & Image(vec1 * mat4));
    Assert_Vec3_Equals(vec1 * mat4, 60.0, 70.0, 80.0);

end Test_Mat4x3;

end Vulkan.Math.Mat4x3.Test;
