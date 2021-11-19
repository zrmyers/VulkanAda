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
with Vulkan.Math.GenDType;

use Vulkan.Math.GenDType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenMatrix
--------------------------------------------------------------------------------
--< @summary
--< This package provides an instantiation of a matrix for the Vkm_Double type.
--<
--< @description
--< The Vkm_GenDMatrix type is a matrix of double precision floating point numbers
--< that can have up to 4 rows and 4 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.GenDMatrix is
    pragma Preelaborate;
    pragma Pure;

    --< @private
    --< GDM is an instantiation of the GenMatrix package for the Vkm_Double type.
    package GDM is new Vulkan.Math.GenMatrix(
        Base_Type        => Vkm_Double,
        Base_Vector_Type => Vkm_GenDType,
        x                => GDT.x,
        y                => GDT.y,
        z                => GDT.z,
        w                => GDT.w,
        Make_GenType     => GDT.Make_GenType,
        Image            => GDT.Image,
        Set              => GDT.Component,
        Get              => GDT.Component);

    --< The Vkm_GenFMatrix is a subtype of Vkm_GenMatrix from the instantiated GDM
    --< package.
    subtype Vkm_Dmat is GDM.Vkm_GenMatrix;

    ----------------------------------------------------------------------------
    --< @summary
    --< Get the image of the Vkm_Dmat matrix.
    --<
    --< @param instance
    --< The instance of matrix to get the image of.
    --<
    --< @return
    --< The image of the matrix.
    ----------------------------------------------------------------------------
    function Image (instance : in     Vkm_Dmat) return String is
        (GDM.Image(instance)) with inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Equality operator for two Vkm_Dmat2x2 matrices.
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
    function "=" (left, right : in     Vkm_Dmat) return Vkm_Bool renames GDM.Op_Is_Equal;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary plus operator for a Vkm_Dmat matrix.
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
    function "+" (right : in     Vkm_Dmat) return Vkm_Dmat is
        (right) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary minus operator for a Vkm_Dmat matrix.
    --<
    --< @description
    --< Return the matrix negated.
    ----------------------------------------------------------------------------
    function "-" is new GDM.Apply_Func_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Unary Absolute Value operator for a Vkm_Dmat matrix.
    --<
    --< @description
    --< Return a matrix of the absolute values of each element of the input matrix.
    ----------------------------------------------------------------------------
    function "abs" is new GDM.Apply_Func_IM_RM("abs");


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for two Vkm_Dmat matrices.
    --<
    --< @description
    --< Perform Modulo component-wise on two Vkm_Dmat matrices.
    ----------------------------------------------------------------------------
    function "mod" is new GDM.Apply_Func_IM_IM_RM("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Modulo component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "mod" is new GDM.Apply_Func_IM_IS_RM("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Modulo operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Modulo component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "mod" is new GDM.Apply_Func_IS_IM_RM("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for two Vkm_Dmat matrices.
    --<
    --< @description
    --< Perform Power component-wise on two Vkm_Dmat matrices.
    ----------------------------------------------------------------------------
    function "**" is new GDM.Apply_Func_IM_IM_RM("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Power component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "**" is new GDM.Apply_Func_IM_IS_RM("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Power operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Power component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "**" is new GDM.Apply_Func_IS_IM_RM("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for two Vkm_Dmat matrices.
    --<
    --< @description
    --< Perform Addition component-wise on two Vkm_Dmat matrices.
    ----------------------------------------------------------------------------
    function "+" is new GDM.Apply_Func_IM_IM_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Addition component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "+" is new GDM.Apply_Func_IM_IS_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Addition operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Addition component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "+" is new GDM.Apply_Func_IS_IM_RM("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for two Vkm_Dmat matrices.
    --<
    --< @description
    --< Perform Subtraction component-wise on two Vkm_Dmat matrices.
    ----------------------------------------------------------------------------
    function "-" is new GDM.Apply_Func_IM_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Subtraction component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "-" is new GDM.Apply_Func_IM_IS_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtraction operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Subtraction component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "-" is new GDM.Apply_Func_IS_IM_RM("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for two Vkm_Dmat matrices.
    --<
    --< @description
    --< Perform Remainder component-wise on two Vkm_Dmat matrices.
    ----------------------------------------------------------------------------
    function "rem" is new GDM.Apply_Func_IM_IM_RM(Vkm_Double'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Remainder component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "rem" is new GDM.Apply_Func_IM_IS_RM(Vkm_Double'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Remainder operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Remainder component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "rem" is new GDM.Apply_Func_IS_IM_RM(Vkm_Double'Remainder);


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for two Vkm_Dmat matrices.
    --<
    --< @description
    --< Perform Division component-wise on two Vkm_Dmat matrices.
    ----------------------------------------------------------------------------
    function "/" is new GDM.Apply_Func_IM_IM_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Division component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "/" is new GDM.Apply_Func_IM_IS_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Division operator for a Vkm_Dmat matrix and a Vkm_Double value.
    --<
    --< @description
    --< Perform Division component-wise on the matrix and scalar.
    ----------------------------------------------------------------------------
    function "/" is new GDM.Apply_Func_IS_IM_RM("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Linear Algebraic Multiplication for two Vkm_Dmat matrices.
    --<
    --< @description
    --< Perform Linear Algebraic Multiplication of the left Vkm_Dmat by the right
    --< Vkm_Dmat matrix.
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
    function "*" (left, right : in     Vkm_Dmat) return Vkm_Dmat renames GDM.Op_Matrix_Mult_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Multiplication operator for a Vkm_GenMatrix matrix and a Vkm_GenDType value.
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
        left  : in     Vkm_Dmat;
        right : in     Vkm_GenDType ) return Vkm_GenDType renames GDM.Op_Matrix_Mult_Vector;


    ----------------------------------------------------------------------------
    --< @summary
    --< Multiplication operator for a Vkm_GenMatrix matrix and a Vkm_GenDType value.
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
        left  : in     Vkm_GenDType;
        right : in     Vkm_Dmat     ) return Vkm_GenDType renames GDM.Op_Vector_Mult_Matrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Multiply a matrix and a scalar.
    --<
    --< @description
    --< Multiplies a left scalar and a right matrix.
    ----------------------------------------------------------------------------
    function "*" is new GDM.Apply_Func_IS_IM_RM("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Multiply a matrix and a scalar.
    --<
    --< @description
    --< Multiplies a right scalar and a left matrix.
    ----------------------------------------------------------------------------
    function "*" is new GDM.Apply_Func_IM_IS_RM("*");


    ----------------------------------------------------------------------------
    -- GLSL Matrix Functions
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @summary
    --< Multiply two matrices component-wise.
    --<
    --< @description
    --< Multiply matrix x by matrix y component-wise, ie, result[i][j] is the
    --< scalar product of x[i][j] and y[i][j].
    --<
    --< @x
    --< Matrix x, must have same dimmensions as y.
    --<
    --< @y
    --< Matrix y, must have same dimmensions as z
    --<
    --< @return
    --< The component-wise product of two matrices.
    ----------------------------------------------------------------------------
    function Matrix_Comp_Mut is new GDM.Apply_Func_IM_IM_RM("*");

end Vulkan.Math.GenDMatrix;
