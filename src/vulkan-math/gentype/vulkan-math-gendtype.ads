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
with Vulkan.Math.GenType;
with Vulkan.Math.GenBType;
with Vulkan.Math.GenIType;

use Vulkan.Math.GenBType;
use Vulkan.Math.GenIType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenType
--------------------------------------------------------------------------------
--< @summary
--< This package describes any length vector of Vkm_Double type.
--<
--< @description
--< Provides an instantiation of the generic GenType  package with a Base_Type of
--< Vkm_Double. This is used to provide the Vkm_GenDType subtype for the Vulkan Math
--< library.
--------------------------------------------------------------------------------
package Vulkan.Math.GenDType is
    pragma Preelaborate;
    pragma Pure;

    --< @private
    --< An instance of the generic GenType package, with Vkm_Double as the Base_Type.
    package GDT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Double,
        Default_Value => 0.0,
        Image => Vkm_Double'Image,
        Unary_Minus => "-",
        Multiply => "*");

    --< A subtype of the instantiated Vkm_GenType that represents the GenDType
    --< described in the GLSL specification.
    subtype Vkm_GenDType is GDT.Vkm_GenType;

    ----------------------------------------------------------------------------
    -- Functions
    ----------------------------------------------------------------------------
    function Image(instance : in     Vkm_GenDType) return String is
        (GDT.Image(GDT.Vkm_GenType(instance))) with inline;


    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Vkm_Double and Vkm_Bool type to GenDType
    --< and GenBType vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenDType vectors and
    --< a GenBType vector returning a GenDType vector.
    --<
    --<    RVD := [Func(IVD1.x, IVD2.x, IVB1.x) ... Func(IVD1.w, IVD2.w, IVB1.w)]
    --<
    --< @param IVD1
    --< The first input GenDType parameter.
    --<
    --< @param IVD2
    --< The second input GenDType parameter.
    --<
    --< @param IVB1
    --< The first input GenBType parameter.
    --<
    --< @return
    --< The result GenDType vector, RVD.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISD1, ISD2 : in     Vkm_Double;
                           ISB1       : in     Vkm_Bool ) return Vkm_Double;
    function Apply_Func_IVD_IVD_IVB_RVD(IVD1, IVD2 : in     Vkm_GenDType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function on a Vkm_Double input that returns a VKm_Bool component-wise
    --< to a GenDType vector.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenDType vector returning
    --< a GenBType vector.
    --<
    --<    RVB := [Func(IVD1.x) ... Func(IVD1.w)]
    --<
    --< @param IVD1
    --< The input GenDType parameter.
    --<
    --< @return
    --< The resulting GenBType vector, RVB.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISD : in     Vkm_Double) return Vkm_Bool;
    function Apply_Func_IVD_RVB(IVD1 : in     Vkm_GenDType) return Vkm_GenBType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function on a Vkm_Double input and a Vkm_Int outut that returns a Vkm_Double
    --< component-wise to the corresponding vector types.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenDType and GenIType
    --< vector, returning a GenDType vector.
    --<
    --<    RVD := [Func(IVD.x,OVI.x) ... Func(IVD.w,OVI.w)]
    --<
    --< @param IVD
    --< The input GenDType parameter.
    --<
    --< @param OVI
    --< The output GenIType parameter.
    --<
    --< @return
    --< The resulting GenDType vector, RVD.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISD : in     Vkm_Double;
                            ISI :    out Vkm_Int) return Vkm_Double;
    function Apply_Func_IVD_OVI_RVD(IVD : in     Vkm_GenDType;
                                    OVI :    out Vkm_GenIType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function on a Vkm_Double and a Vkm_Int input that returns a Vkm_Double
    --< component-wise to the corresponding vector types.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenDType and GenIType
    --< vector, returning a GenDType vector.
    --<
    --<    RVD := [Func(IVD.x,IVI.x) ... Func(IVD.w,IVI.w)]
    --<
    --< @param IVD
    --< The input GenDType parameter.
    --<
    --< @param IVI
    --< The input GenIType parameter.
    --<
    --< @return
    --< The resulting GenDType vector, RVD.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISD : in     Vkm_Double;
                            ISI : in     Vkm_Int) return Vkm_Double;
    function Apply_Func_IVD_IVI_RVD(IVD : in     Vkm_GenDType;
                                    IVI : in     Vkm_GenIType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function on two Vkm_Double inputs that returns a Vkm_Bool component-wise
    --< to two Vkm_GenDType vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenDType vectors,
    --< returning a GenBType vector.
    --<
    --<    RVB := [Func(IVD1.x,IVD2.x) ... Func(IVD1.w,IVD2.w)]
    --<
    --< @param IVD1
    --< The first input GenDType parameter.
    --<
    --< @param IVD2
    --< The second input GenDType parameter.
    --<
    --< @return
    --< The resulting GenBType vector, RVB.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISD1 : in     Vkm_Double;
                            ISD2 : in     Vkm_Double) return Vkm_Bool;
    function Apply_Func_IVD_IVD_RVB(IVD1, IVD2 : in     Vkm_GenDType) return Vkm_GenBType;

    ----------------------------------------------------------------------------
    -- The Operators for double precision floating point vectors are defined here.
    --
    -- A summary of operators that can be used with GenDType values of different
    -- size:
    --    - "&", Concatenation
    --
    -- A summary of operators that are component-wise Unary:
    --    - "+"  , Unary plus operator.
    --    - "-"  , Unary minus operator.
    --    - "abs", Absolute value operator.
    --
    -- A summary of operators that are component-wise on two input vectors of the
    -- same length. Additionally, a scalar may appear instead of a vector on the
    -- left or right hand side of these operators:
    --    - "mod", Modulus operator.
    --    - "**",  Power operator.
    --    - "+",   Addition operator.
    --    - "-",   Subtraction operator.
    --    - "rem", Remainder operator.
    --    - "*",   Multiplication operator.
    --    - "/",   Division operator.
    --
    -- A summary of relational operators that are component-wise on two input vecotrs
    -- of the same length, and return a vector of booleans of the same length:
    --    - "<",  Less than operator
    --    - ">",  Greater than operator
    --    - "<=", Less than or equal to operator
    --    - ">=", Greater than or equal to operator
    --
    -- A summary of relational operators which return a scalar boolean value.
    --    - "=",  Equality operator
    --    - "/=", Non-Equality operator
    --
    ----------------------------------------------------------------------------
    -- GenDType Concatenation Operators
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @summary
    --< GenDType concatenation operator.
    --<
    --< @description
    --< Concatenate two GenDType vectors.
    --<    vector := vector & vector
    --<
    --< @param left
    --< Parameter to the left of the '&' symbol.
    --<
    --< @param right
    --< Parameter to the right of the '&' symbol.
    --<
    --< @return
    --< Append right vector to left vector.
    ----------------------------------------------------------------------------
    function "&" (left, right : in     Vkm_GenDType) return Vkm_GenDType renames GDT.Concatenate;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType concatenation operator.
    --<
    --< @description
    --< Concatenate a Vkm_Double and a Vkm_GenDType vector.
    --<    vector := scalar & vector
    --<
    --< @param left
    --< Parameter to the left of the '&' symbol.
    --<
    --< @param right
    --< Parameter to the right of the '&' symbol.
    --<
    --< @return
    --< Append right vector to left scalar.
    ----------------------------------------------------------------------------
    function "&" (left        : in     Vkm_Double   ;
                  right       : in     Vkm_GenDType) return Vkm_GenDType is
        (GDT.Make_GenType(left).Concatenate(right)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType concatenation operator.
    --<
    --< @description
    --< Concatenate a Vkm_Double and a Vkm_GenDType vector.
    --<    vector := vector & scalar
    --<
    --< @param left
    --< Parameter to the left of the '&' symbol.
    --<
    --< @param right
    --< Parameter to the right of the '&' symbol.
    --<
    --< @return
    --< Append right scalar to left vector.
    ----------------------------------------------------------------------------
    function "&" (left        : in     Vkm_GenDType;
                  right       : in     Vkm_Double   ) return Vkm_GenDType is
        (left.Concatenate(GDT.Make_GenType(right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType concatenation operator.
    --<
    --< @description
    --< Concatenate two Vkm_Doubles to form a Vkm_GenDType vector.
    --<    vector := scalar & scalar
    --<
    --< @param left
    --< Parameter to the left of the '&' symbol.
    --<
    --< @param right
    --< Parameter to the right of the '&' symbol.
    --<
    --< @return
    --< Append right scalar to left scalar.
    ----------------------------------------------------------------------------
    function "&" (left, right : in     Vkm_Double   ) return Vkm_GenDType is
        (GDT.Make_GenType(left, right)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType unary plus operator.
    --<
    --< @description
    --< Return the unmodified vector.
    --<     vector :=  +vector;
    ----------------------------------------------------------------------------
    function "+" is new GDT.Apply_Func_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType unary minus operator.
    --<
    --< @description
    --< Return the negation of the vector.
    --<     vector :=  -vector;
    ----------------------------------------------------------------------------
    function "-" is new GDT.Apply_Func_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType absolute value operator.
    --<
    --< @description
    --< Return the absolute value for each component of the vector.
    --<     vector :=  abs vector;
    ----------------------------------------------------------------------------
    function "abs" is new GDT.Apply_Func_IV_RV("abs");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType modulo operator.
    --<
    --< @description
    --< Return the component-wise modulo of the two vectors.
    --<     vector :=  vector mod vector;
    ----------------------------------------------------------------------------
    function "mod" is new GDT.Apply_Func_IV_IV_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType modulo operator.
    --<
    --< @description
    --< Return the component-wise modulo of a vector and a scalar.
    --<     vector :=  vector mod scalar;
    ----------------------------------------------------------------------------
    function "mod" is new GDT.Apply_Func_IV_IS_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType modulo operator.
    --<
    --< @description
    --< Return the component-wise modulo of a vector and a scalar.
    --<     vector :=  scalar mod vector;
    ----------------------------------------------------------------------------
    function "mod" is new GDT.Apply_Func_IS_IV_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType power operator.
    --<
    --< @description
    --< Apply the power operation component-wise on two vectors.
    --<    vector := vector ** vector;
    ----------------------------------------------------------------------------
    function "**" is new GDT.Apply_Func_IV_IV_RV("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType power operator.
    --<
    --< @description
    --< Apply the power operation component-wise on a vector and a scalar.
    --<    vector := vector ** scalar;
    ----------------------------------------------------------------------------
    function "**" is new GDT.Apply_Func_IV_IS_RV("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType power operator.
    --<
    --< @description
    --< Apply the power operation component-wise on a vector and a scalar.
    --<    vector := scalar ** vector;
    ----------------------------------------------------------------------------
    function "**" is new GDT.Apply_Func_IS_IV_RV("**");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType addition operator.
    --<
    --< @description
    --< Apply the additon operation component-wise on two vectors.
    --<    vector := vector + vector;
    ----------------------------------------------------------------------------
    function "+" is new GDT.Apply_Func_IV_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType addition operator.
    --<
    --< @description
    --< Apply the additon operation component-wise on a vector and a scalar.
    --<    vector := vector + scalar;
    ----------------------------------------------------------------------------
    function "+" is new GDT.Apply_Func_IV_IS_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType addition operator.
    --<
    --< @description
    --< Apply the additon operation component-wise on a vector and a scalar.
    --<    vector := scalar + vector;
    ----------------------------------------------------------------------------
    function "+" is new GDT.Apply_Func_IS_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType subtraction operator.
    --<
    --< @description
    --< Apply the subtraction operation component-wise on two vectors.
    --<    vector := vector - vector;
    ----------------------------------------------------------------------------
    function "-" is new GDT.Apply_Func_IV_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType subtraction operator.
    --<
    --< @description
    --< Apply the subtraction operation component-wise on a vector and a scalar.
    --<    vector := vector - scalar;
    ----------------------------------------------------------------------------
    function "-" is new GDT.Apply_Func_IV_IS_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType subtraction operator.
    --<
    --< @description
    --< Apply the subtraction operation component-wise on a vector and a scalar.
    --<    vector := scalar - vector;
    ----------------------------------------------------------------------------
    function "-" is new GDT.Apply_Func_IS_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType remainder operator.
    --<
    --< @description
    --< Calculate the remainder of division of left by right:
    --<     scalar := left rem right;
    --<
    --< @param left
    --< The numerator in the division for which the remainder is returned.
    --<
    --< @param right
    --< The denominator in the division for which the remainder is returned.
    --<
    --< @return
    --< The remainder of left divided by right.
    ----------------------------------------------------------------------------
    function "rem" (left, right : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Remainder;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType remainder operator.
    --<
    --< @description
    --< Calculate the remainder of division of for two vectors.
    --<     vector := vector rem vector;
    ----------------------------------------------------------------------------
    function "rem" is new GDT.Apply_Func_IV_IV_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType remainder operator.
    --<
    --< @description
    --< Calculate the remainder of division of for a vector and a scalar.
    --<     vector := vector rem scalar;
    ----------------------------------------------------------------------------
    function "rem" is new GDT.Apply_Func_IV_IS_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType remainder operator.
    --<
    --< @description
    --< Calculate the remainder of division of for a vector and a scalar.
    --<     vector := scalar rem vector;
    ----------------------------------------------------------------------------
    function "rem" is new GDT.Apply_Func_IS_IV_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType multiplication operator.
    --<
    --< @description
    --< Apply the multiplication operation component-wise on two vectors.
    --<    vector := vector * vector;
    ----------------------------------------------------------------------------
    function "*" is new GDT.Apply_Func_IV_IV_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType multiplication operator.
    --<
    --< @description
    --< Apply the multiplication operation component-wise on a vector and a scalar.
    --<    vector := vector * scalar;
    ----------------------------------------------------------------------------
    function "*" is new GDT.Apply_Func_IV_IS_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType multiplication operator.
    --<
    --< @description
    --< Apply the multiplication operation component-wise on a vector and a scalar.
    --<    vector := scalar * vector;
    ----------------------------------------------------------------------------
    function "*" is new GDT.Apply_Func_IS_IV_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType division operator.
    --<
    --< @description
    --< Calculate the component-wise division of two vectors.
    --<     vector := vector / vector;
    ----------------------------------------------------------------------------
    function "/" is new GDT.Apply_Func_IV_IV_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType division operator.
    --<
    --< @description
    --< Calculate the component-wise division of a vector and a scalar.
    --<     vector := vector / scalar;
    ----------------------------------------------------------------------------
    function "/" is new GDT.Apply_Func_IV_IS_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType division operator.
    --<
    --< @description
    --< Calculate the component-wise division of a vector and a scalar.
    --<     vector := scalar / vector;
    ----------------------------------------------------------------------------
    function "/" is new GDT.Apply_Func_IS_IV_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType less than operator.
    --<
    --< @description
    --< Return the result of the component-wise less than operator on two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function "<"  (left, right : in     Vkm_GenDType) return Vkm_GenBType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType less than or equal to operator.
    --<
    --< @description
    --< Return the result of the component-wise less than or equal to operator on
    --< two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function "<=" (left, right : in     Vkm_GenDType) return Vkm_GenBType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType greater than operator.
    --<
    --< @description
    --< Return the result of the component-wise greater than operator on
    --< two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function ">"  (left, right : in     Vkm_GenDType) return Vkm_GenBType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType greater than or equal operator.
    --<
    --< @description
    --< Return the result of the component-wise greater than or equal to operator
    --< on two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function ">=" (left, right : in     Vkm_GenDType) return Vkm_GenBType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType equality operator.
    --<
    --< @description
    --< Return true if each component of two vectors are equal to each other.
    --< Otherwise return false.
    --<     is_equal := vector = vector;
    --<
    --< @param left
    --< The left vector in the comparison.
    --<
    --< @param right
    --< The right vector in the comparison.
    --<
    --< @return
    --< Whether the two vectors are equal to each other.
    ----------------------------------------------------------------------------
    function "="  (left, right : in     Vkm_GenDType) return Vkm_Bool renames GDT.Equals;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType inequality operator.
    --<
    --< @description
    --< Return true if any component of the two vectors are not equal to each other.
    --< Otherwise return false.
    --<     is_equal := vector /= vector;
    --<
    --< @param left
    --< The left vector in the comparison.
    --<
    --< @param right
    --< The right vector in the comparison.
    --<
    --< @return
    --< Whether the two vectors are equal to each other.
    ----------------------------------------------------------------------------
    function "/=" (left, right : in     Vkm_GenDType) return Vkm_Bool is
        (not (left = right)) with Inline;

end Vulkan.Math.GenDType;
