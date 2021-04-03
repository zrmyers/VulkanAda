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
with Vulkan.Math.GenFMatrix;
with Vulkan.Math.Vec4;

use Vulkan.Math.GenFMatrix;
use Vulkan.Math.Vec4;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides a single precision floating point matrix with 2 rows
--< and 4 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.Mat3x4 is
    pragma Preelaborate;
    pragma Pure;

    --< A 2x3 matrix of single-precision floating point numbers.
    subtype Vkm_Mat3x4 is Vkm_Mat(
        last_row_index => 2, last_column_index => 3);


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Mat3x4 type.
    --<
    --< @description
    --< Construct a 2x3 matrix with each component set to zero.
    --<
    --< @return
    --< A 2x3 matrix.
    ----------------------------------------------------------------------------
    function Make_Mat3x4 return Vkm_Mat3x4 is
        (GFM.Make_GenMatrix(cN => 3, rN => 2)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Mat3x4 type.
    --<
    --< @description
    --< Construct a 2x3 matrix with each component set to a different value.
    --<
    --<     | value1 value3 value5 value7 |
    --<     | value2 value4 value6 value8 |
    --<
    --< @param value1
    --< The first value to set for the matrix.
    --<
    --< @param value2
    --< The second value to set for the matrix.
    --<
    --< @param value3
    --< The third value to set for the matrix.
    --<
    --< @param value4
    --< The fourth value to set for the matrix.
    --<
    --< @param value5
    --< The fifth value to set for the matrix.
    --<
    --< @param value6
    --< The sixth value to set for the matrix.
    --<
    --< @param value7
    --< The seventh value to set for the matrix.
    --<
    --< @param value8
    --< The eighth value to set for the matrix.
    --<
    --< @param value9
    --< The ninth value to set for the matrix.
    --<
    --< @param value10
    --< The tenth value to set for the matrix.
    --<
    --< @param value11
    --< The eleventh value to set for the matrix.
    --<
    --< @return
    --< A 3x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Mat3x4 (
        value1, value2 , value3 , value4 ,
        value5, value6 , value7 , value8 ,
        value9, value10, value11, value12 : in     Vkm_Float) return Vkm_Mat3x4 is
        (GFM.Make_GenMatrix(
             cN => 3, rN => 2,
             c0r0_val => value1, c0r1_val => value5, c0r2_val => value9,
             c1r0_val => value2, c1r1_val => value6, c1r2_val => value10,
             c2r0_val => value3, c2r1_val => value7, c2r2_val => value11,
             c3r0_val => value4, c3r1_val => value8, c3r2_val => value12)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Mat3x4 type.
    --<
    --< @description
    --< Construct a 3x4 matrix with each row set to the value of a 4 dimmensional
    --< vector.
    --<
    --<     | value1.x value1.y value1.z value1.w |
    --<     | value2.x value2.y value2.z value2.w |
    --<
    --< @param value1
    --< The first value to set for the matrix.
    --<
    --< @param value2
    --< The second value to set for the matrix.
    --<
    --< @return
    --< A 3x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Mat3x4 (
        value1, value2, value3: in     Vkm_Vec4) return Vkm_Mat3x4 is
        (GFM.Make_GenMatrix(
             cN => 3, rN => 2,
             c0r0_val => value1.x, c0r1_val => value2.x, c0r2_val => value3.x,
             c1r0_val => value1.y, c1r1_val => value2.y, c1r2_val => value3.y,
             c2r0_val => value1.z, c2r1_val => value2.z, c2r2_val => value3.z,
             c3r0_val => value1.w, c3r1_val => value2.w, c3r2_val => value3.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Mat3x4 type.
    --<
    --< @description
    --< Construct a 2x3 matrix using values from an existing matrix.
    --<
    --< If the provided matrix has dimmensions that are not the same as this
    --< matrix, the corresponding element in the 4x4 identity matrix is used for
    --< out of bounds accesses.
    --<
    --< @param value1
    --< The submatrix to extract values from.
    --<
    --< @return
    --< A 3x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Mat3x4 (
        value1 : in     Vkm_Mat) return Vkm_Mat3x4 is
        (GFM.Make_GenMatrix(
             cN => 3, rN => 2,
             c0r0_val => value1.c0r0, c0r1_val => value1.c0r1, c0r2_val => value1.c0r2,
             c1r0_val => value1.c1r0, c1r1_val => value1.c1r1, c1r2_val => value1.c1r2,
             c2r0_val => value1.c2r0, c2r1_val => value1.c2r1, c2r2_val => value1.c2r2,
             c3r0_val => value1.c3r0, c3r1_val => value1.c3r1, c3r2_val => value1.c3r2)) with Inline;


end Vulkan.Math.Mat3x4;
