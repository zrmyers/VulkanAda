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
with Vulkan.Math.GenDMatrix;
with Vulkan.Math.Dvec4;

use Vulkan.Math.GenDMatrix;
use Vulkan.Math.Dvec4;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides a single precision floating point matrix with 3 rows
--< and 3 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.Dmat4x4 is
    pragma Preelaborate;
    pragma Pure;

    --< A 4x4 matrix of single-precision floating point numbers.
    subtype Vkm_Dmat4x4 is Vkm_Dmat(
        last_row_index => 3, last_column_index => 3);

    --< An alternative name for a 4x4 single-precision floating point matrix
    subtype Vkm_Dmat4 is Vkm_Dmat4x4;

    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat4x4 type.
    --<
    --< @description
    --< Construct a 4x4 matrix with each component set to the corresponding
    --< component in the identity matrix.
    --<
    --< @return
    --< A 4x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat4x4 return Vkm_Dmat4x4 is
        (GDM.Make_GenMatrix(cN => 3, rN => 3, diag => 1.0)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat4x4 type.
    --<
    --< @description
    --< Construct a 4x4 matrix with each component on the diagonal set to a
    --< particular value.
    --<
    --< @param diag
    --< The value to set along the diagonal.
    --<
    --< @return
    --< A 4x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat4x4 (
        diag : in     Vkm_Double) return Vkm_Dmat4x4 is
        (GDM.Make_GenMatrix(cN => 3, rN => 3, diag => diag)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat4x4 type.
    --<
    --< @description
    --< Construct a 4x4 matrix with each component on the diagonal set to a
    --< particular value from a vector.
    --<
    --< @param diag
    --< The value to set along the diagonal.
    --<
    --< @return
    --< A 4x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat4x4 (
        diag : in     Vkm_Dvec4) return Vkm_Dmat4x4 is
        (GDM.Make_GenMatrix(cN => 3, rN => 3, diag => diag)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat4x4 type.
    --<
    --< @description
    --< Construct a 4x4 matrix with each component set to a different value.
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
    --< The eighth value to set  for the matrix.
    --<
    --< @param value9
    --< The ninth value to set for the matrix.
    --<
    --< @return
    --< A 4x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat4x4 (
        value1 , value2 , value3 , value4 ,
        value5 , value6 , value7 , value8 ,
        value9 , value10, value11, value12,
        value13, value14, value15, value16 : in     Vkm_Double) return Vkm_Dmat4x4 is
        (GDM.Make_GenMatrix(
             cN => 3, rN => 3,
             c0r0_val => value1, c0r1_val => value5, c0r2_val => value9 , c0r3_val => value13,
             c1r0_val => value2, c1r1_val => value6, c1r2_val => value10, c1r3_val => value14,
             c2r0_val => value3, c2r1_val => value7, c2r2_val => value11, c2r3_val => value15,
             c3r0_val => value4, c3r1_val => value8, c3r2_val => value12, c3r3_val => value16)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat4x4 type.
    --<
    --< @description
    --< Construct a 4x4 matrix with each column set to the value of a 2 dimmensional
    --< vector.
    --<
    --< @param value1
    --< The first value to set for the matrix.
    --<
    --< @param value2
    --< The second value to set for the matrix.
    --<
    --< @return
    --< A 4x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat4x4 (
        value1, value2, value3, value4 : in     Vkm_Dvec4) return Vkm_Dmat4x4 is
        (GDM.Make_GenMatrix(
             cN => 3, rN => 3,
             c0r0_val => value1.x, c0r1_val => value2.x, c0r2_val => value3.x, c0r3_val => value4.x,
             c1r0_val => value1.y, c1r1_val => value2.y, c1r2_val => value3.y, c1r3_val => value4.y,
             c2r0_val => value1.z, c2r1_val => value2.z, c2r2_val => value3.z, c2r3_val => value4.z,
             c3r0_val => value1.w, c3r1_val => value2.w, c3r2_val => value3.w, c3r3_val => value4.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat4x4 type.
    --<
    --< @description
    --< Construct a 4x4 matrix using values from an existing matrix.
    --<
    --< If the provided matrix has dimmensions that are not the same as this
    --< matrix, the corresponding element in the 4x4 identity matrix is used for
    --< out of bounds accesses.
    --<
    --< @param value1
    --< The submatrix to extract values from.
    --<
    --< @return
    --< A 4x4 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat4x4 (
        value1 : in     Vkm_Dmat) return Vkm_Dmat4x4 is
        (GDM.Make_GenMatrix(
             cN => 3, rN => 3,
             c0r0_val => value1.c0r0, c0r1_val => value1.c0r1, c0r2_val => value1.c0r2, c0r3_val => value1.c0r3,
             c1r0_val => value1.c1r0, c1r1_val => value1.c1r1, c1r2_val => value1.c1r2, c1r3_val => value1.c1r3,
             c2r0_val => value1.c2r0, c2r1_val => value1.c2r1, c2r2_val => value1.c2r2, c2r3_val => value1.c2r3,
             c3r0_val => value1.c3r0, c3r1_val => value1.c3r1, c3r2_val => value1.c3r2, c3r3_val => value1.c3r3)) with Inline;


end Vulkan.Math.Dmat4x4;
