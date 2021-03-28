--------------------------------------------------------------------------------
-- MIT License
--
-- Copyright (c) 2020 Zane Myers
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
use Ada.Text_IO;

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

with Vulkan.Math.Mat2x3;
use Vulkan.Math.Mat2x3;

with Vulkan.Math.Vec2;
use Vulkan.Math.Vec2;

with Vulkan.Math.Vec3;
use Vulkan.Math.Vec3;

with Vulkan.Math.Operators;
use Vulkan.Math.Operators;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides tests for single precision floating point mat2x3.
--------------------------------------------------------------------------------
package body Vulkan.Math.Mat2x3.Test is

procedure Assert_Vkm_Bool_Equals(
    actual : in Vkm_Bool;
    expected : in Vkm_Bool) is

    is_failure : Boolean := True;
begin
    is_failure := not( actual = expected);

    if is_failure then
        Put_Line("Assertion FAIL!");
        Put_Line("    actual = " & actual'Image);
        Put_Line("    expected = " & expected'Image);
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Vkm_Bool_Equals;

procedure Assert_Mat2x3_Equals(
    mat : in Vkm_Mat2x3;
    value1, value2, value3, value4, value5, value6 : in Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c2r0 = value3 and
                       mat.c0r1 = value4 and
                       mat.c1r1 = value5 and
                       mat.c2r1 = value6);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image & ", " & value3'Image & " ], " &
                  "[ " & value4'Image & ", " & value5'Image & ", " & value6'Image & " ]]");
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat2x3_Equals;

-- Test Mat2x3
procedure Test_Mat2x3 is

    vec1 : Vkm_Vec2 :=
        Make_Vec2(1.0, 2.0);

    vec2 : Vkm_Vec3 :=
        Make_Vec3(1.0, 2.0, 3.0);

    mat1 : Vkm_Mat2x3 :=
        Make_Mat2x3;

    mat2 : Vkm_Mat2x3 :=
        Make_Mat2x3(0.0, 1.0, 2.0, 3.0, 4.0, 5.0);

    mat3 : Vkm_Mat2x3 :=
        Make_Mat2x3(vec2, - vec2);

    mat4 : Vkm_Mat2x3 :=
        Make_Mat2x3(mat2);

    mat5 : Vkm_Mat2x2 :=
        Make_Mat2x2(5.0);

    mat6 : Vkm_Mat2x3 :=
        Make_Mat2x3(mat5);

begin

    Put_Line(LF & "Testing Mat2x3 Constructors...");

    Put_Line("mat1 " & mat1.Image);
    Assert_Mat2x3_Equals(mat1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

    Put_Line("mat2 " & mat2.Image);
    Assert_Mat2x3_Equals(mat2, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0);

    Put_Line("mat3 " & mat3.Image);
    Assert_Mat2x3_Equals(mat3, 1.0, 2.0, 3.0, -1.0, -2.0, -3.0);

    Put_Line("mat4 " & mat4.Image);
    Assert_Mat2x3_Equals(mat4, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0);

    Put_Line("mat6 " & mat6.Image);
    Assert_Mat2x3_Equals(mat6, 5.0, 0.0, 0.0, 0.0, 5.0, 0.0);

    Put_Line("Testing '=' operator...");
    Put_Line(" mat2 != mat3");
    Assert_Vkm_Bool_Equals(mat2 = mat3, False);

    Put_Line(" mat4 != mat5");
    Assert_Vkm_Bool_Equals(mat4 = mat5, False);

    Put_Line(" mat4 = mat2");
    Assert_Vkm_Bool_Equals(mat4 = mat2, True);

    Put_Line(" Testing unary '+/-' operator");
    Put_Line(" + mat4 = " & Image(+ mat4));
    Assert_Mat2x3_Equals(+mat4, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0);

    Put_Line(" - mat4 = " & Image(- mat4));
    Assert_Mat2x3_Equals(-mat4, -0.0, -1.0, -2.0, -3.0, -4.0, -5.0);

    Put_Line("+(- mat4) = " & Image(+(- mat4)));
    Assert_Mat2x3_Equals(-mat4, -0.0, -1.0, -2.0, -3.0, -4.0, -5.0);

    Put_Line("Testing 'abs' operator...");
    Put_Line(" abs(- mat4) = " & Image(abs(-mat4)));
    Assert_Mat2x3_Equals(abs(-mat4), 0.0, 1.0, 2.0, 3.0, 4.0, 5.0);

    Put_Line("Testing '+' operator...");
    Put_Line(" mat4 + mat3 = " & Image(mat4 + mat3));
    Assert_Mat2x3_Equals(mat4 + mat3, 1.0, 3.0, 5.0, 2.0, 2.0, 2.0);

    Put_Line("Testing '-' operator...");
    Put_Line(" mat4 - mat3 = " & Image(mat4 -mat3));
    Assert_Mat2x3_Equals(mat4 - mat3, -1.0, -1.0, -1.0, 4.0, 6.0, 8.0);

end Test_Mat2x3;

end Vulkan.Math.Mat2x3.Test;
