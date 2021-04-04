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
with Vulkan.Math;
with Vulkan.Math.Vec2;
with Vulkan.Math.Vec3;
with Vulkan.Math.Vec4;
with Vulkan.Math.Mat2x2;
with Vulkan.Math.Mat2x3;
with Vulkan.Math.Mat2x4;
with Vulkan.Math.Mat3x2;
with Vulkan.Math.Mat3x3;
with Vulkan.Math.Mat3x4;
with Vulkan.Math.Mat4x2;
with Vulkan.Math.Mat4x3;

use Vulkan.Math;
use Vulkan.Math.Vec2;
use Vulkan.Math.Vec3;
use Vulkan.Math.Vec4;
use Vulkan.Math.Mat2x2;
use Vulkan.Math.Mat2x3;
use Vulkan.Math.Mat2x4;
use Vulkan.Math.Mat3x2;
use Vulkan.Math.Mat3x3;
use Vulkan.Math.Mat3x4;
use Vulkan.Math.Mat4x2;
use Vulkan.Math.Mat4x3;

--------------------------------------------------------------------------------
--< @group Vulkan Test Framwork
--------------------------------------------------------------------------------
--< @summary
--< This package provides a simple framework for validating the VulkanAda library.
--------------------------------------------------------------------------------
package Vulkan.Test.Framework is

-- An exception that is raised when a failure is observed.
VULKAN_TEST_ASSERTION_FAIL : exception;

procedure Assert_Vkm_Bool_Equals(
    actual : in Vkm_Bool;
    expected : in Vkm_Bool);

procedure Assert_Vec2_Equals(
    vec : in Vkm_Vec2;
    value1, value2 : in Vkm_Float);

procedure Assert_Vec3_Equals(
    vec : in Vkm_Vec3;
    value1, value2, value3 : in Vkm_Float);

procedure Assert_Vec4_Equals(
    vec : in Vkm_Vec4;
    value1, value2, value3, value4 : in Vkm_Float);

procedure Assert_Mat2x2_Equals(
    mat : in Vkm_Mat2;
    value1, value2,
    value3, value4 : in Vkm_Float);

procedure Assert_Mat2x3_Equals(
    mat : in Vkm_Mat2x3;
    value1, value2, value3,
    value4, value5, value6 : in Vkm_Float);

procedure Assert_Mat2x4_Equals(
    mat : in Vkm_Mat2x4;
    value1, value2, value3, value4,
    value5, value6, value7, value8 : in Vkm_Float);

procedure Assert_Mat3x2_Equals(
    mat : in Vkm_Mat3x2;
    value1, value2,
    value3, value4,
    value5, value6 : in Vkm_Float);

procedure Assert_Mat3x3_Equals(
    mat : in Vkm_Mat3;
    value1, value2, value3,
    value4, value5, value6,
    value7, value8, value9 : in Vkm_Float);

procedure Assert_Mat3x4_Equals(
    mat : in Vkm_Mat3x4;
    value1, value2 , value3 , value4 ,
    value5, value6 , value7 , value8 ,
    value9, value10, value11, value12 : in Vkm_Float);

procedure Assert_Mat4x2_Equals(
    mat : in Vkm_Mat4x2;
    value1, value2,
    value3, value4,
    value5, value6,
    value7, value8 : in Vkm_Float);

procedure Assert_Mat4x3_Equals(
    mat : in Vkm_Mat4x3;
    value1 , value2 , value3 ,
    value4 , value5 , value6 ,
    value7 , value8 , value9 ,
    value10, value11, value12 : in Vkm_Float);

end Vulkan.Test.Framework;
