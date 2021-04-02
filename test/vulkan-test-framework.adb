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
with Vulkan.Math.Mat2x4;
with Vulkan.Math.Vec2;

use Ada.Text_IO;
use Ada.Characters.Latin_1;
use Vulkan.Math.Mat2x4;
use Vulkan.Math.Vec2;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides tests for single precision floating point mat2x4.
--------------------------------------------------------------------------------
package body Vulkan.Test.Framework is

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
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Vkm_Bool_Equals;


--------------------------------------------------------------------------------


procedure Assert_Vec2_Equals(
    vec            : in     Vkm_Vec2;
    value1, value2 : in     Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (vec.x = value1 and
                       vec.y = value2);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    actual = " & vec.Image);
        Put_Line("    expected = [ " & value1'Image & ", " & value2'Image & " ]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Vec2_Equals;


--------------------------------------------------------------------------------


procedure Assert_Mat2x2_Equals(
    mat : in Vkm_Mat2;
    value1, value2, value3, value4 : in Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c0r1 = value3 and
                       mat.c1r1 = value4);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", "  &value2'Image & " ], " &
                  "[ " & value3'Image & "," & value4'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat2x2_Equals;


--------------------------------------------------------------------------------


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
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat2x3_Equals;


--------------------------------------------------------------------------------


procedure Assert_Mat2x4_Equals(
    mat : in Vkm_Mat2x4;
    value1, value2, value3, value4,
    value5, value6, value7, value8 : in Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c2r0 = value3 and
                       mat.c3r0 = value4 and
                       mat.c0r1 = value5 and
                       mat.c1r1 = value6 and
                       mat.c2r1 = value7 and
                       mat.c3r1 = value8);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image & ", " & value3'Image & value4'Image & " ], " &
                  "[ " & value5'Image & ", " & value6'Image & ", " & value7'Image & value8'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat2x4_Equals;


--------------------------------------------------------------------------------


procedure Assert_Mat3x2_Equals(
    mat : in Vkm_Mat3x2;
    value1, value2,
    value3, value4,
    value5, value6 : in Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c0r1 = value3 and
                       mat.c1r1 = value4 and
                       mat.c0r2 = value5 and
                       mat.c1r2 = value6);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image & " ], " &
                  "[ " & value3'Image & ", " & value4'Image & " ], " &
                  "[ " & value5'Image & ", " & value6'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat3x2_Equals;


--------------------------------------------------------------------------------


procedure Assert_Mat3x3_Equals(
    mat : in Vkm_Mat3;
    value1, value2, value3,
    value4, value5, value6,
    value7, value8, value9 : in Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c2r0 = value3 and
                       mat.c0r1 = value4 and
                       mat.c1r1 = value5 and
                       mat.c2r1 = value6 and
                       mat.c0r2 = value7 and
                       mat.c1r2 = value8 and
                       mat.c2r2 = value9);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image & ", " & value3'Image & " ], " &
                  "[ " & value4'Image & ", " & value5'Image & ", " & value6'Image & " ], " &
                  "[ " & value7'Image & ", " & value8'Image & ", " & value9'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat3x3_Equals;


end Vulkan.Test.Framework;
