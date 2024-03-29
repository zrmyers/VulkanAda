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
with Vulkan.Math.Dvec2;

use Vulkan.Math.GenDMatrix;
use Vulkan.Math.Dvec2;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides a single precision floating point matrix with 2 rows
--< and 2 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.Dmat2x2 is
    pragma Preelaborate;
    pragma Pure;

    --< A 2x2 matrix of single-precision floating point numbers.
    subtype Vkm_Dmat2x2 is Vkm_Dmat(
        last_row_index => 1, last_column_index => 1);

    --< An alternative name for a 2x2 single-precision floating point matrix
    subtype Vkm_Dmat2 is Vkm_Dmat2x2;

    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x2 type.
    --<
    --< @description
    --< Construct a 2x2 matrix with each component set to the corresponding
    --< component in the identity matrix.
    --<
    --< @return
    --< A 2x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x2 return Vkm_Dmat2x2 is
        (GDM.Make_GenMatrix(cN => 1, rN => 1, diag => 1.0)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x2 type.
    --<
    --< @description
    --< Construct a 2x2 matrix with each component on the diagonal set to a
    --< particular value.
    --<
    --< @param diag
    --< The value to set along the diagonal.
    --<
    --< @return
    --< A 2x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x2 (
        diag : in     Vkm_Double) return Vkm_Dmat2x2 is
        (GDM.Make_GenMatrix(cN => 1, rN => 1, diag => diag)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x2 type.
    --<
    --< @description
    --< Construct a 2x2 matrix with each component on the diagonal set to a
    --< particular value from a 2 dimmensional vector.
    --<
    --< @param diag
    --< The value to set along the diagonal.
    --<
    --< @return
    --< A 2x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x2 (
        diag : in     Vkm_Dvec2) return Vkm_Dmat2x2 is
        (GDM.Make_GenMatrix(cN => 1, rN => 1, diag => diag)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x2 type.
    --<
    --< @description
    --< Construct a 2x2 matrix with each component set to a different value.
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
    --< @return
    --< A 2x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x2 (
        value1, value2, value3, value4 : in     Vkm_Double) return Vkm_Dmat2x2 is
        (GDM.Make_GenMatrix(
             cN => 1, rN => 1,
             c0r0_val => value1, c0r1_val => value3,
             c1r0_val => value2, c1r1_val => value4)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x2 type.
    --<
    --< @description
    --< Construct a 2x2 matrix with each row set to the value of a 2 dimmensional
    --< vector.
    --<
    --< @param value1
    --< The first value to set for the matrix.
    --<
    --< @param value2
    --< The second value to set for the matrix.
    --<
    --< @return
    --< A 2x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x2 (
        value1, value2 : in     Vkm_Dvec2) return Vkm_Dmat2x2 is
        (GDM.Make_GenMatrix(
             cN => 1, rN => 1,
             c0r0_val => value1.x, c0r1_val => value2.x,
             c1r0_val => value1.y, c1r1_val => value2.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dmat2x2 type.
    --<
    --< @description
    --< Construct a 2x2 matrix using values from an existing matrix.
    --<
    --< If the provided matrix has dimmensions that are not the same as this
    --< matrix, the corresponding element in the 4x4 identity matrix is used for
    --< out of bounds accesses.
    --<
    --< @param value1
    --< The submatrix to extract values from.
    --<
    --< @return
    --< A 2x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Dmat2x2 (
        value1 : in     Vkm_Dmat) return Vkm_Dmat2x2 is
        (GDM.Make_GenMatrix(
             cN => 1, rN => 1,
             c0r0_val => value1.c0r0, c0r1_val => value1.c0r1,
             c1r0_val => value1.c1r0, c1r1_val => value1.c1r1)) with Inline;


end Vulkan.Math.Dmat2x2;
