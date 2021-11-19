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
with Vulkan.Math.GenDMatrix;
with Vulkan.Math.Dmat2x2;
with Vulkan.Math.Dmat2x4;
with Vulkan.Math.GenDType;
with Vulkan.Math.Dvec2;
with Vulkan.Math.Dvec4;
with Vulkan.Math.Operators;
with Vulkan.Test.Framework;

use Ada.Text_IO;
use Ada.Characters.Latin_1;
use Vulkan.Math.Dmat2x2;
use Vulkan.Math.Dmat2x4;
use Vulkan.Math.GenDType;
use Vulkan.Math.Dvec2;
use Vulkan.Math.Dvec4;
use Vulkan.Test.Framework;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides tests for single precision floating point mat2x4.
--------------------------------------------------------------------------------
package body Vulkan.Math.Dmat2x4.Test is

-- Test Mat2x4
procedure Test_Dmat2x4 is

    vec1 : Vkm_Dvec2 :=
        Make_Dvec2(1.0, 2.0);

    vec2 : Vkm_Dvec4 :=
        Make_Dvec4(1.0, 2.0, 3.0, 4.0);

    mat1 : Vkm_Dmat2x4 :=
        Make_Dmat2x4;

    mat2 : Vkm_Dmat2x4 :=
        Make_Dmat2x4(0.0, 1.0, 2.0, 3.0,
                    4.0, 5.0, 6.0, 7.0);

    mat3 : Vkm_Dmat2x4 :=
        Make_Dmat2x4(vec2, - vec2);

    mat4 : Vkm_Dmat2x4 :=
        Make_Dmat2x4(mat2);

    mat5 : Vkm_Dmat2x2 :=
        Make_Dmat2x2(5.0);

    mat6 : Vkm_Dmat2x4 :=
        Make_Dmat2x4(mat5);

begin

    Put_Line(LF & "Testing Mat2x4 Constructors...");

    Put_Line("mat1 " & mat1.Image);
    Assert_Dmat2x4_Equals(mat1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

    Put_Line("mat2 " & mat2.Image);
    Assert_Dmat2x4_Equals(mat2, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0);

    Put_Line("mat3 " & mat3.Image);
    Assert_Dmat2x4_Equals(mat3, 1.0, 2.0, 3.0, 4.0, -1.0, -2.0, -3.0, -4.0);

    Put_Line("mat4 " & mat4.Image);
    Assert_Dmat2x4_Equals(mat4, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0);

    Put_Line("mat6 " & mat6.Image);
    Assert_Dmat2x4_Equals(mat6, 5.0, 0.0, 0.0, 0.0, 0.0, 5.0, 0.0, 0.0);

    Put_Line("Testing '=' operator...");
    Put_Line(" mat2 != mat3");
    Assert_Vkm_Bool_Equals(mat2 = mat3, False);

    Put_Line(" mat4 != mat5");
    Assert_Vkm_Bool_Equals(mat4 = mat5, False);

    Put_Line(" mat4 = mat2");
    Assert_Vkm_Bool_Equals(mat4 = mat2, True);

    Put_Line(" Testing unary '+/-' operator");
    Put_Line(" + mat4 = " & Image(+ mat4));
    Assert_Dmat2x4_Equals(+mat4, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0);

    Put_Line(" - mat4 = " & Image(- mat4));
    Assert_Dmat2x4_Equals(-mat4, -0.0, -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0);

    Put_Line("+(- mat4) = " & Image(+(- mat4)));
    Assert_Dmat2x4_Equals(-mat4, -0.0, -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0);

    Put_Line("Testing 'abs' operator...");
    Put_Line(" abs(- mat4) = " & Image(abs(-mat4)));
    Assert_Dmat2x4_Equals(abs(-mat4), 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0);

    Put_Line("Testing '+' operator...");
    Put_Line(" mat4 + mat3 = " & Image(mat4 + mat3));
    Assert_Dmat2x4_Equals(mat4 + mat3, 1.0, 3.0, 5.0, 7.0, 3.0, 3.0, 3.0, 3.0);

    Put_Line("Testing '-' operator...");
    Put_Line(" mat4 - mat3 = " & Image(mat4 -mat3));
    Assert_Dmat2x4_Equals(mat4 - mat3, -1.0, -1.0, -1.0, -1.0, 5.0, 7.0, 9.0, 11.0);

    Put_Line("Testing '*' operator...");
    Put_Line(" mat5 * mat4 = " & Image(mat5 * mat4));
    Assert_Dmat2x4_Equals(mat5 * mat4, 0.0 , 5.0 , 10.0, 15.0,
                                      20.0, 25.0, 30.0, 35.0);

    Put_Line(" mat4 * vec2 = " & Image(mat4 * vec2));
    Assert_Dvec2_Equals(mat4 * vec2, 20.0, 60.0);

    Put_Line(" vec1 * mat4 = " & Image(vec1 * mat4));
    Assert_Dvec4_Equals(vec1 * mat4, 8.0, 11.0, 14.0, 17.0);


end Test_Dmat2x4;

end Vulkan.Math.Dmat2x4.Test;
