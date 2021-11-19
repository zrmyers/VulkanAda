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
with Vulkan.Math.GenDMatrix;
with Vulkan.Math.Dvec3;

use Vulkan.Math.GenDMatrix;
use Vulkan.Math.Dvec3;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides a double precision floating point matrix with 2 rows
--< and 3 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.Dmat2x3 is
    pragma Preelaborate;
    pragma Pure;

    --< A 2x3 matrix of double-precision floating point numbers.
    subtype Vkm_Dmat2x3 is Vkm_Dmat(
        last_row_index => 1, last_column_index => 2);


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x3 type.
    --<
    --< @description
    --< Construct a 2x3 matrix with each component set to zero.
    --<
    --< @return
    --< A 2x3 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x3 return Vkm_Dmat2x3 is
        (GDM.Make_GenMatrix(cN => 2, rN => 1)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x3 type.
    --<
    --< @description
    --< Construct a 2x3 matrix with each component set to a different value.
    --<
    --<     | value1 value3 value5 |
    --<     | value2 value4 value6 |
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
    --< The Sixth value to set for the matrix.
    --<
    --< @return
    --< A 2x3 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x3 (
        value1, value2,
        value3, value4,
        value5, value6 : in     Vkm_Double) return Vkm_Dmat2x3 is
        (GDM.Make_GenMatrix(
             cN => 2, rN => 1,
             c0r0_val => value1, c0r1_val => value4,
             c1r0_val => value2, c1r1_val => value5,
             c2r0_val => value3, c2r1_val => value6)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x3 type.
    --<
    --< @description
    --< Construct a 2x3 matrix with each row set to the value of a 3 dimmensional
    --< vector.
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
    --< @return
    --< A 2x3 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x3 (
        value1, value2 : in     Vkm_Dvec3) return Vkm_Dmat2x3 is
        (GDM.Make_GenMatrix(
             cN => 2, rN => 1,
             c0r0_val => value1.x, c0r1_val => value2.x,
             c1r0_val => value1.y, c1r1_val => value2.y,
             c2r0_val => value1.z, c2r1_val => value2.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x3 type.
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
    --< A 2x3 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x3 (
        value1 : in     Vkm_Dmat) return Vkm_Dmat2x3 is
        (GDM.Make_GenMatrix(
             cN => 2, rN => 1,
             c0r0_val => value1.c0r0, c0r1_val => value1.c0r1,
             c1r0_val => value1.c1r0, c1r1_val => value1.c1r1,
             c2r0_val => value1.c2r0, c2r1_val => value1.c2r1)) with Inline;


end Vulkan.Math.Dmat2x3;
