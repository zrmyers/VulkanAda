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
with Vulkan.Math.Mat2x2;
with Vulkan.Math.Vec2;
with Vulkan.Math.Vec3;

use Vulkan.Math.GenFMatrix;
use Vulkan.Math.Mat2x2;
use Vulkan.Math.Vec2;
use Vulkan.Math.Vec3;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides a single precision floating point matrix with 3 rows
--< and 2 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.Mat3x2 is
    pragma Preelaborate;
    pragma Pure;

    --< A 3x2 matrix of single-precision floating point numbers.
    subtype Vkm_Mat3x2 is Vkm_Mat(
        last_row_index => 2, last_column_index => 1);

    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Mat3x2 type.
    --<
    --< @description
    --< Construct a 3x2 matrix with each component set to 0.0
    --<
    --< @return
    --< A 3x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Mat3x2 return Vkm_Mat3x2 is
        (GFM.Make_GenMatrix(cN => 1, rN => 2)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Mat3x2 type.
    --<
    --< @description
    --< Construct a 3x2 matrix with each component set to a different value.
    --<
    --<     | value1 value4 |
    --<     | value2 value5 |
    --<     | value3 value6 |
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
    --< @return
    --< A 3x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Mat3x2 (
        value1, value2, value3,
        value4, value5, value6 : in     Vkm_Float) return Vkm_Mat3x2 is
        (GFM.Make_GenMatrix(
             cN => 1, rN => 2,
             c0r0_val => value1, c0r1_val => value3, c0r2_val => value5,
             c1r0_val => value2, c1r1_val => value4, c1r2_val => value6)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Mat3x2 type.
    --<
    --< @description
    --< Construct a 3x2 matrix with each row set to the value of a 2 dimmensional
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
    --< A 3x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Mat3x2 (
        value1, value2, value3 : in     Vkm_Vec2) return Vkm_Mat3x2 is
        (GFM.Make_GenMatrix(
             cN => 1, rN => 2,
             c0r0_val => value1.x, c0r1_val => value2.x, c0r2_val => value3.x,
             c1r0_val => value1.y, c1r1_val => value2.y, c1r2_val => value3.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Mat3x2 type.
    --<
    --< @description
    --< Construct a 3x2 matrix using values from an existing matrix.
    --<
    --< If the provided matrix has dimmensions that are not the same as this
    --< matrix, the corresponding element in the 4x4 identity matrix is used for
    --< out of bounds accesses.
    --<
    --< @param value1
    --< The submatrix to extract values from.
    --<
    --< @return
    --< A 3x2 matrix.
    ----------------------------------------------------------------------------
    function Make_Mat3x2 (
        value1 : in     Vkm_Mat) return Vkm_Mat3x2 is
        (GFM.Make_GenMatrix(
             cN => 1, rN => 2,
             c0r0_val => value1.c0r0, c0r1_val => value1.c0r1, c0r2_val => value1.c0r2,
             c1r0_val => value1.c1r0, c1r1_val => value1.c1r1, c1r2_val => value1.c1r2)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Equality operator for two Vkm_Mat3x2 matrices.
    --<
    --< @description
    --< Determine whether two matrices are equal.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< True if the two matrices are equal. Otherwise false.
    ----------------------------------------------------------------------------
    function "=" (left, right : in     Vkm_Mat3x2) return Vkm_Bool renames GFM.Op_Is_Equal;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary Plus operator on a Vkm_Mat3x2.
    --<
    --< @description
    --< Perform Unary Plus operation on the matrix, returning the matrix unchanged.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The matrix, unchanged.
    ----------------------------------------------------------------------------
    function "+" (right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Plus_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary Minus operator on a Vkm_Mat3x2.
    --<
    --< @description
    --< Perform Unary Minus operation on the matrix, returning the matrix with
    --< each element negated.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The matrix, negated.
    ----------------------------------------------------------------------------
    function "-" (right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Minus_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Absolute Value operator on a Vkm_Mat3x2.
    --<
    --< @description
    --< Perform the Absolute Value operation on the matrix, returning the matrix with
    --< each each negative element negated.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The matrix, with all positive elements.
    ----------------------------------------------------------------------------
    function "abs" (right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Abs_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for two Vkm_Mat3x2 matrices.
    --<
    --< @description
    --< Perform Modulo between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "mod" (left, right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Matrix_Mod_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Modulo component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "mod" (left  : in     Vkm_Mat3x2;
                    right : in     Vkm_Float ) return Vkm_Mat3x2 renames Op_Matrix_Mod_Scalar;


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Modulo component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "mod" (left  : in     Vkm_Float;
                    right : in     Vkm_Mat3x2 ) return Vkm_Mat3x2 renames Op_Scalar_Mod_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for two Vkm_Mat3x2 matrices.
    --<
    --< @description
    --< Perform Power operation component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "**" (left, right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Matrix_Pow_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform power component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "**" (left  : in     Vkm_Mat3x2;
                   right : in     Vkm_Float ) return Vkm_Mat3x2 renames Op_Matrix_Pow_Scalar;


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform power component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "**" (left  : in     Vkm_Float;
                   right : in     Vkm_Mat3x2 ) return Vkm_Mat3x2 renames Op_Scalar_Pow_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for two Vkm_Mat3x2 matrices.
    --<
    --< @description
    --< Perform Addition between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The left argument added to the right argument.
    ----------------------------------------------------------------------------
    function "+" (left, right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Matrix_Plus_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Addition component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "+" (left  : in     Vkm_Mat3x2;
                  right : in     Vkm_Float ) return Vkm_Mat3x2 renames Op_Matrix_Plus_Scalar;


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Addition component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "+" (left  : in     Vkm_Float;
                  right : in     Vkm_Mat3x2 ) return Vkm_Mat3x2 renames Op_Scalar_Plus_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for two Vkm_Mat3x2 matrices.
    --<
    --< @description
    --< Perform Subtraction between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "-" (left, right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Matrix_Minus_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Subtraction component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "-" (left  : in     Vkm_Mat3x2;
                  right : in     Vkm_Float ) return Vkm_Mat3x2 renames Op_Matrix_Minus_Scalar;


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Subtraction component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "-" (left  : in     Vkm_Float;
                  right : in     Vkm_Mat3x2 ) return Vkm_Mat3x2 renames Op_Scalar_Minus_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for two Vkm_Mat3x2 matrices.
    --<
    --< @description
    --< Perform Remainder between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "rem" (left, right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Matrix_Rem_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Remainder component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "rem" (left  : in     Vkm_Mat3x2;
                    right : in     Vkm_Float ) return Vkm_Mat3x2 renames Op_Matrix_Rem_Scalar;


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Remainder component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "rem" (left  : in     Vkm_Float;
                    right : in     Vkm_Mat3x2 ) return Vkm_Mat3x2 renames Op_Scalar_Rem_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for two Vkm_Mat3x2 matrices.
    --<
    --< @description
    --< Perform Division between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "/" (left, right : in     Vkm_Mat3x2) return Vkm_Mat3x2 renames Op_Matrix_Div_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Division component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "/" (left  : in     Vkm_Mat3x2;
                  right : in     Vkm_Float ) return Vkm_Mat3x2 renames Op_Matrix_Div_Scalar;


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for a Vkm_Mat3x2 matrix and a Vkm_Float scalar.
    --<
    --< @description
    --< Perform Division component-wise between the left and right input arguments.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "/" (left  : in     Vkm_Float;
                  right : in     Vkm_Mat3x2 ) return Vkm_Mat3x2 renames Op_Scalar_Div_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Linear Algebraic Multiplication for a Vkm_Mat2x2 matrix with a Vkm_Mat2x2
    --< matrix.
    --<
    --< @description
    --< Perform Linear Algebraic Multiplication of the left Vkm_Mat3x2 by the right
    --< Vkm_Mat2x2 matrix.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "*" (
        left  : in     Vkm_Mat3x2;
        right : in     Vkm_Mat2x2) return Vkm_Mat3x2 renames GFM.Op_Matrix_Mult_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Linear Algebraic Multiplication for a Vkm_Mat3x2 with a vector.
    --<
    --< @description
    --< Perform Linear Algebraic Multiplication of the left Vkm_Mat3x2 by the right
    --< Vkm_Vec2 vector.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "*" (left  : in     Vkm_Mat3x2;
                  right : in     Vkm_Vec2  ) return Vkm_Vec3 renames Op_Matrix_Mult_Vector;


    ----------------------------------------------------------------------------
    --< @summary
    --< Linear Algebraic Multiplication for a Vkm_Mat3x2 with a vector.
    --<
    --< @description
    --< Perform Linear Algebraic Multiplication of the left Vkm_Vec3 by the right
    --< Vkm_Mat3x2.
    --<
    --< @param left
    --< The left operator argument.
    --<
    --< @param right
    --< The right operator argument.
    --<
    --< @return
    --< The result of the component-wise operator on the arguments.
    ----------------------------------------------------------------------------
    function "*" (left  : in     Vkm_Vec3;
                  right : in     Vkm_Mat3x2  ) return Vkm_Vec2 renames Op_Vector_Mult_Matrix;


end Vulkan.Math.Mat3x2;