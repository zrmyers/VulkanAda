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


procedure Assert_Vec3_Equals(
    vec            : in     Vkm_Vec3;
    value1, value2, value3 : in     Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (vec.x = value1 and
                       vec.y = value2 and
                       vec.z = value3);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    actual = " & vec.Image);
        Put_Line("    expected = [ " & value1'Image & ", " & value2'Image & ", " & value3'Image & " ]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Vec3_Equals;


--------------------------------------------------------------------------------


procedure Assert_Vec4_Equals(
    vec                            : in     Vkm_Vec4;
    value1, value2, value3, value4 : in     Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (vec.x = value1 and
                       vec.y = value2 and
                       vec.z = value3 and
                       vec.w = value4);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    actual = " & vec.Image);
        Put_Line("    expected = [ " & value1'Image & ", " & value2'Image & ", " & value3'Image & ", " & value4'Image & " ]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Vec4_Equals;


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


--------------------------------------------------------------------------------


procedure Assert_Mat3x4_Equals(
    mat : in Vkm_Mat3x4;
    value1, value2, value3, value4,
    value5, value6, value7, value8,
    value9, value10, value11, value12 : in Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c2r0 = value3 and
                       mat.c3r0 = value4 and
                       mat.c0r1 = value5 and
                       mat.c1r1 = value6 and
                       mat.c2r1 = value7 and
                       mat.c3r1 = value8 and
                       mat.c0r2 = value9 and
                       mat.c1r2 = value10 and
                       mat.c2r2 = value11 and
                       mat.c3r2 = value12);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image  & ", " & value3'Image  & ", " & value4'Image  & " ], " &
                  "[ " & value5'Image & ", " & value6'Image  & ", " & value7'Image  & ", " & value8'Image  & " ], " &
                  "[ " & value9'Image & ", " & value10'Image & ", " & value11'Image & ", " & value12'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat3x4_Equals;


--------------------------------------------------------------------------------


procedure Assert_Mat4x2_Equals(
    mat : in Vkm_Mat4x2;
    value1, value2,
    value3, value4,
    value5, value6,
    value7, value8 : in Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c0r1 = value3 and
                       mat.c1r1 = value4 and
                       mat.c0r2 = value5 and
                       mat.c1r2 = value6 and
                       mat.c0r3 = value7 and
                       mat.c1r3 = value8);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image & " ], " &
                  "[ " & value3'Image & ", " & value4'Image & " ], " &
                  "[ " & value5'Image & ", " & value6'Image & " ], " &
                  "[ " & value7'Image & ", " & value8'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat4x2_Equals;


--------------------------------------------------------------------------------


procedure Assert_Mat4x3_Equals(
    mat : in Vkm_Mat4x3;
    value1,  value2,  value3,
    value4,  value5,  value6,
    value7,  value8,  value9,
    value10, value11, value12 : in Vkm_Float) is

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
                       mat.c2r2 = value9 and
                       mat.c0r3 = value10 and
                       mat.c1r3 = value11 and
                       mat.c2r3 = value12);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image  & ", " & value2'Image  & ", " & value3'Image  & " ], " &
                  "[ " & value4'Image  & ", " & value5'Image  & ", " & value6'Image  & " ], " &
                  "[ " & value7'Image  & ", " & value8'Image  & ", " & value9'Image  & " ], " &
                  "[ " & value10'Image & ", " & value11'Image & ", " & value12'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat4x3_Equals;


--------------------------------------------------------------------------------


procedure Assert_Mat4x4_Equals(
    mat : in Vkm_Mat4x4;
    value1, value2, value3, value4,
    value5, value6, value7, value8,
    value9, value10, value11, value12,
    value13, value14, value15, value16 : in Vkm_Float) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c2r0 = value3 and
                       mat.c3r0 = value4 and
                       mat.c0r1 = value5 and
                       mat.c1r1 = value6 and
                       mat.c2r1 = value7 and
                       mat.c3r1 = value8 and
                       mat.c0r2 = value9 and
                       mat.c1r2 = value10 and
                       mat.c2r2 = value11 and
                       mat.c3r2 = value12 and
                       mat.c0r3 = value13 and
                       mat.c1r3 = value14 and
                       mat.c2r3 = value15 and
                       mat.c3r3 = value16);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image  & ", " & value3'Image  & ", " & value4'Image  & " ], " &
                  "[ " & value5'Image & ", " & value6'Image  & ", " & value7'Image  & ", " & value8'Image  & " ], " &
                  "[ " & value9'Image & ", " & value10'Image & ", " & value11'Image & ", " & value12'Image & " ], " &
                  "[ " & value13'Image & ", " & value14'Image & ", " & value15'Image & ", " & value16'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Mat4x4_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dvec2_Equals(
    vec            : in     Vkm_Dvec2;
    value1, value2 : in     Vkm_Double) is

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
end Assert_Dvec2_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dvec3_Equals(
    vec            : in     Vkm_Dvec3;
    value1, value2, value3 : in     Vkm_Double) is

    is_failure : Boolean := True;
begin
    is_failure := not (vec.x = value1 and
                       vec.y = value2 and
                       vec.z = value3);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    actual = " & vec.Image);
        Put_Line("    expected = [ " & value1'Image & ", " & value2'Image & ", " & value3'Image & " ]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Dvec3_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dvec4_Equals(
    vec                            : in     Vkm_Dvec4;
    value1, value2, value3, value4 : in     Vkm_Double) is

    is_failure : Boolean := True;
begin
    is_failure := not (vec.x = value1 and
                       vec.y = value2 and
                       vec.z = value3 and
                       vec.w = value4);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    actual = " & vec.Image);
        Put_Line("    expected = [ " & value1'Image & ", " & value2'Image & ", " & value3'Image & ", " & value4'Image & " ]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Dvec4_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat2x2_Equals(
    mat : in Vkm_Dmat2;
    value1, value2, value3, value4 : in Vkm_Double) is

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
end Assert_Dmat2x2_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat2x3_Equals(
    mat : in Vkm_Dmat2x3;
    value1, value2, value3, value4, value5, value6 : in Vkm_Double) is

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
end Assert_Dmat2x3_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat2x4_Equals(
    mat : in Vkm_Dmat2x4;
    value1, value2, value3, value4,
    value5, value6, value7, value8 : in Vkm_Double) is

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
end Assert_Dmat2x4_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat3x2_Equals(
    mat : in Vkm_Dmat3x2;
    value1, value2,
    value3, value4,
    value5, value6 : in Vkm_Double) is

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
end Assert_Dmat3x2_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat3x3_Equals(
    mat : in Vkm_Dmat3;
    value1, value2, value3,
    value4, value5, value6,
    value7, value8, value9 : in Vkm_Double) is

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
end Assert_Dmat3x3_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat3x4_Equals(
    mat : in Vkm_Dmat3x4;
    value1, value2, value3, value4,
    value5, value6, value7, value8,
    value9, value10, value11, value12 : in Vkm_Double) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c2r0 = value3 and
                       mat.c3r0 = value4 and
                       mat.c0r1 = value5 and
                       mat.c1r1 = value6 and
                       mat.c2r1 = value7 and
                       mat.c3r1 = value8 and
                       mat.c0r2 = value9 and
                       mat.c1r2 = value10 and
                       mat.c2r2 = value11 and
                       mat.c3r2 = value12);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image  & ", " & value3'Image  & ", " & value4'Image  & " ], " &
                  "[ " & value5'Image & ", " & value6'Image  & ", " & value7'Image  & ", " & value8'Image  & " ], " &
                  "[ " & value9'Image & ", " & value10'Image & ", " & value11'Image & ", " & value12'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Dmat3x4_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat4x2_Equals(
    mat : in Vkm_Dmat4x2;
    value1, value2,
    value3, value4,
    value5, value6,
    value7, value8 : in Vkm_Double) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c0r1 = value3 and
                       mat.c1r1 = value4 and
                       mat.c0r2 = value5 and
                       mat.c1r2 = value6 and
                       mat.c0r3 = value7 and
                       mat.c1r3 = value8);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image & " ], " &
                  "[ " & value3'Image & ", " & value4'Image & " ], " &
                  "[ " & value5'Image & ", " & value6'Image & " ], " &
                  "[ " & value7'Image & ", " & value8'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Dmat4x2_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat4x3_Equals(
    mat : in Vkm_Dmat4x3;
    value1,  value2,  value3,
    value4,  value5,  value6,
    value7,  value8,  value9,
    value10, value11, value12 : in Vkm_Double) is

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
                       mat.c2r2 = value9 and
                       mat.c0r3 = value10 and
                       mat.c1r3 = value11 and
                       mat.c2r3 = value12);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image  & ", " & value2'Image  & ", " & value3'Image  & " ], " &
                  "[ " & value4'Image  & ", " & value5'Image  & ", " & value6'Image  & " ], " &
                  "[ " & value7'Image  & ", " & value8'Image  & ", " & value9'Image  & " ], " &
                  "[ " & value10'Image & ", " & value11'Image & ", " & value12'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Dmat4x3_Equals;


--------------------------------------------------------------------------------


procedure Assert_Dmat4x4_Equals(
    mat : in Vkm_Dmat4x4;
    value1, value2, value3, value4,
    value5, value6, value7, value8,
    value9, value10, value11, value12,
    value13, value14, value15, value16 : in Vkm_Double) is

    is_failure : Boolean := True;
begin
    is_failure := not (mat.c0r0 = value1 and
                       mat.c1r0 = value2 and
                       mat.c2r0 = value3 and
                       mat.c3r0 = value4 and
                       mat.c0r1 = value5 and
                       mat.c1r1 = value6 and
                       mat.c2r1 = value7 and
                       mat.c3r1 = value8 and
                       mat.c0r2 = value9 and
                       mat.c1r2 = value10 and
                       mat.c2r2 = value11 and
                       mat.c3r2 = value12 and
                       mat.c0r3 = value13 and
                       mat.c1r3 = value14 and
                       mat.c2r3 = value15 and
                       mat.c3r3 = value16);

    if is_failure = True then
        Put_Line("Assertion FAIL!");
        Put_Line("    Actual mat = " & mat.Image);
        Put_Line("    Expected mat = " &
                 "[[ " & value1'Image & ", " & value2'Image  & ", " & value3'Image  & ", " & value4'Image  & " ], " &
                  "[ " & value5'Image & ", " & value6'Image  & ", " & value7'Image  & ", " & value8'Image  & " ], " &
                  "[ " & value9'Image & ", " & value10'Image & ", " & value11'Image & ", " & value12'Image & " ], " &
                  "[ " & value13'Image & ", " & value14'Image & ", " & value15'Image & ", " & value16'Image & " ]]");
        raise VULKAN_TEST_ASSERTION_FAIL;
    else
        Put_Line("Assertion PASS!");
    end if;
end Assert_Dmat4x4_Equals;



end Vulkan.Test.Framework;
