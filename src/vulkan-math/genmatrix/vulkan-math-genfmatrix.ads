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
with Vulkan.Math.Common;
with Vulkan.Math.Exp;

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
        Make_GenType     => GFT.Make_GenType);
        
    --< The Vkm_GenFMatrix is a subtype of Vkm_GenMatrix from the instantiated GFM
    --< package.
    subtype Vkm_Mat is GFM.Vkm_GenMatrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary plus operator for a Vkm_Mat matrix.
    --<
    --< @description
    --< Return the matrix unchanged.
    ----------------------------------------------------------------------------
    function Op_Plus_Matrix (right : in     Vkm_Mat) return Vkm_Mat is
        (right) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary minus operator for a Vkm_Mat matrix.
    --<
    --< @description
    --< Return the matrix negated.
    ----------------------------------------------------------------------------
    function Op_Minus_Matrix is new GFM.Apply_Func_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary Absolute Value operator for a Vkm_Mat matrix.
    --<
    --< @description
    --< Return a matrix of the absolute values of each element of the input matrix.
    ----------------------------------------------------------------------------
    function Op_Abs_Matrix is new GFM.Apply_Func_IM_RM("abs");


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Modulo component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function Op_Matrix_Mod_Matrix is new GFM.Apply_Func_IM_IM_RM(Vulkan.Math.Common.Modulo);


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Modulo component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Matrix_Mod_Scalar is new GFM.Apply_Func_IM_IS_RM(Vulkan.Math.Common.Modulo);


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Modulo component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Scalar_Mod_Matrix is new GFM.Apply_Func_IS_IM_RM(Vulkan.Math.Common.Modulo);


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Power component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function Op_Matrix_Pow_Matrix is new GFM.Apply_Func_IM_IM_RM(Vulkan.Math.Exp.Pow);


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Power component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Matrix_Pow_Scalar is new GFM.Apply_Func_IM_IS_RM(Vulkan.Math.Exp.Pow);


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Power component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Scalar_Pow_Matrix is new GFM.Apply_Func_IS_IM_RM(Vulkan.Math.Exp.Pow);


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Addition component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function Op_Matrix_Plus_Matrix is new GFM.Apply_Func_IM_IM_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Addition component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Matrix_Plus_Scalar is new GFM.Apply_Func_IM_IS_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Addition component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Scalar_Plus_Matrix is new GFM.Apply_Func_IS_IM_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Subtraction component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function Op_Matrix_Minus_Matrix is new GFM.Apply_Func_IM_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Subtraction component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Matrix_Minus_Scalar is new GFM.Apply_Func_IM_IS_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Subtraction component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Scalar_Minus_Matrix is new GFM.Apply_Func_IS_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Remainder component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function Op_Matrix_Rem_Matrix is new GFM.Apply_Func_IM_IM_RM(Vkm_Float'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Remainder component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Matrix_Rem_Scalar is new GFM.Apply_Func_IM_IS_RM(Vkm_Float'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Remainder component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Scalar_Rem_Matrix is new GFM.Apply_Func_IS_IM_RM(Vkm_Float'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for two Vkm_Mat matrices.
    --<
    --< @description
    --< Perform Division component-wise on two Vkm_Mat matrices.
    ----------------------------------------------------------------------------
    function Op_Matrix_Div_Matrix is new GFM.Apply_Func_IM_IM_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Division component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Matrix_Div_Scalar is new GFM.Apply_Func_IM_IS_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for a Vkm_Mat matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Division component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Scalar_Div_Matrix is new GFM.Apply_Func_IS_IM_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_GenMatrix matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Remainder component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Matrix_Mult_Vector (
        left  : in     Vkm_Mat;
        right : in     Vkm_GenFType ) return Vkm_GenFType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_GenMatrix matrix and a Vkm_Float value.
    --<
    --< @description
    --< Perform Remainder component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function Op_Vector_Mult_Matrix (
        left  : in     Vkm_GenFType;
        right : in     Vkm_Mat     ) return Vkm_GenFType;


end Vulkan.Math.GenFMatrix;

