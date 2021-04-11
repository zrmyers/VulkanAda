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
with Vulkan.Math.GenMatrix;
with Vulkan.Math.GenFType;

use Vulkan.Math.GenFType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenMatrix
--------------------------------------------------------------------------------
--< @summary
--< This provides an instantiation of a generic matrix for the Vkm_Float type.
--<
--< @description
--< The Vkm_Matrix type is a generic floating point matrix that can contain up to
--< 4 rows and 4 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.GenFMatrix is
    pragma Preelaborate;
    pragma Pure;


    --< @private
    --< GFM is an instantiation of the GenMatrix package for the Vkm_Float type.
    package GFM is new Vulkan.Math.GenMatrix(
        Base_Type        => Vkm_Float,
        Base_Vector_Type => Vkm_GenFType,
        x                => GFT.x,
        y                => GFT.y,
        z                => GFT.z,
        w                => GFT.w,
        Make_GenType     => GFT.Make_GenType,
        Image            => GFT.Image);

    --< The Vkm_GenFMatrix is a subtype of Vkm_GenMatrix from the instantiated GFM
    --< package.
    subtype Vkm_Mat is GFM.Vkm_GenMatrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Get the image of the Vkm_Mat matrix.
    --<
    --< @param instance
    --< The instance of matrix to get the image of.
    --<
    --< @return
    --< The image of the matrix.
    ----------------------------------------------------------------------------
    function Image (instance : in     Vkm_Mat) return String is
        (GFM.Image(instance)) with inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Equality operator for two Vkm_Mat2x2 matrices.
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
    function "=" (left, right : in     Vkm_Mat) return Vkm_Bool renames GFM.Op_Is_Equal;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary plus operator for a Vkm_Mat matrix.
    --<
    --< @description
    --< Return the matrix unchanged.
    --<
    --< @param right
    --< The value to apply the unary operator to.
    --<
    --< @return
    --< The result of the operator on the matrix.
    ----------------------------------------------------------------------------
    function "+" (right : in     Vkm_Mat) return Vkm_Mat is
        (right) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary minus operator for a Vkm_Mat matrix.
    --<
    --< @description
    --< Return the matrix negated.
    ----------------------------------------------------------------------------
    function "-" is new GFM.Apply_Func_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary Absolute Value operator for a Vkm_Mat matrix.
    --<
    --< @description
    --< Return a matrix of the absolute values of each element of the input matrix.
    ----------------------------------------------------------------------------
    function "abs" is new GFM.Apply_Func_IM_RM("abs");


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Modulo component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function "mod" is new GFM.Apply_Func_IM_IM_RM("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Modulo component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "mod" is new GFM.Apply_Func_IM_IS_RM("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Modulo component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "mod" is new GFM.Apply_Func_IS_IM_RM("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Power component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function "**" is new GFM.Apply_Func_IM_IM_RM("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Power component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "**" is new GFM.Apply_Func_IM_IS_RM("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Power component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "**" is new GFM.Apply_Func_IS_IM_RM("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Addition component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function "+" is new GFM.Apply_Func_IM_IM_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Addition component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "+" is new GFM.Apply_Func_IM_IS_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Addition component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "+" is new GFM.Apply_Func_IS_IM_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Subtraction component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function "-" is new GFM.Apply_Func_IM_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Subtraction component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "-" is new GFM.Apply_Func_IM_IS_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Subtraction component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "-" is new GFM.Apply_Func_IS_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Remainder component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function "rem" is new GFM.Apply_Func_IM_IM_RM(Vkm_Float'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Remainder component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "rem" is new GFM.Apply_Func_IM_IS_RM(Vkm_Float'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Remainder component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "rem" is new GFM.Apply_Func_IS_IM_RM(Vkm_Float'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Division component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function "/" is new GFM.Apply_Func_IM_IM_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Division component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "/" is new GFM.Apply_Func_IM_IS_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Division component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "/" is new GFM.Apply_Func_IS_IM_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Linear Algebraic Multiplication for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Linear Algebraic Multiplication of the left Vkm_Mat by the right
    --< Vkm_Mat matrix.
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
    function "*" (left, right : in     Vkm_Mat) return Vkm_Mat renames GFM.Op_Matrix_Mult_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Multiplication operator for a Vkm_GenMatrix matrix and a Vkm_GenFType value.
    --<
    --< @description
    --< Perform Multiplication component-wise on the matrix and vector.
    --<
    --< @param left
    --< The matrix that is multiplied with the vector.
    --<
    --< @param right
    --< The vector that is multiplied by the matrix.
    --<
    --< @return
    --< The product of the matrix with the vector.
    ----------------------------------------------------------------------------
    function "*" (
        left  : in     Vkm_Mat;
        right : in     Vkm_GenFType ) return Vkm_GenFType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Multiplication operator for a Vkm_GenMatrix matrix and a Vkm_GenFType value.
    --<
    --< @description
    --< Perform Multiplication component-wise on the matrix and vector.
    --<
    --< @param left
    --< The vector that is multiplied with the matrix.
    --<
    --< @param right
    --< The matrix that is multiplied by the vector.
    --<
    --< @return
    --< The product of the vector with the matrix.
    ----------------------------------------------------------------------------
    function "*" (
        left  : in     Vkm_GenFType;
        right : in     Vkm_Mat     ) return Vkm_GenFType;


end Vulkan.Math.GenFMatrix;
