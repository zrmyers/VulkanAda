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
with Vulkan.Math.GenFType;
with Vulkan.Math.GenDType;
with Vulkan.Math.GenUType;
with Vulkan.Math.GenIType;
with Vulkan.Math.GenBType;
with Vulkan.Math.Exp;
with Vulkan.Math.Common;
with Vulkan.Math.Relational;

use Vulkan.Math.GenFType;
use Vulkan.Math.GenDType;
use Vulkan.Math.GenUType;
use Vulkan.Math.GenIType;
use Vulkan.Math.GenBType;
use Vulkan.Math.Relational;

--------------------------------------------------------------------------------
--< @group Vulkan Math Operators
--------------------------------------------------------------------------------
--< @summary
--< This package provides operator definitions for all basic vector and matrix 
--< types.
--------------------------------------------------------------------------------
package Vulkan.Math.Operators is
    pragma Preelaborate;
    pragma Pure;


    ----------------------------------------------------------------------------
    -- The Operators for boolean vectors are defined here.
    --
    -- A summary of operators that can be used with GenBType values of different
    -- size:
    --    - "&", Concatenation
    --
    -- A summary of operators that are component-wise Unary:
    --    - "not", Bitwise complement operator
    --
    -- A summary of operators that are component-wise on two input vectors of the
    -- same length. Additionally, a scalar may appear instead of a vector on the
    -- left or right hand side of these operators:
    --    - "and", Bitwise AND operator.
    --    - "or" , Bitwise OR operator.
    --    - "xor", Bitwise XOR operator.
    --
    -- A summary of relational operators which return a scalar boolean value.
    --    - "=",  Equality operator
    --    - "/=", Non-Equality operator
    --
    ----------------------------------------------------------------------------
    -- GenBType Concatenation Operators
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType concatenation operator.
    --<
    --< @description
    --< Concatenate two Vkm_GenBType vectors.
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
    function "&" (left, right : in     Vkm_GenBType) return Vkm_GenBType renames GBT.Concatenate;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType concatenation operator.
    --<
    --< @description
    --< Concatenate a scalar Vkm_Bool and a Vkm_GenBType vector.
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
    function "&" (left        : in     Vkm_Bool   ;
                  right       : in     Vkm_GenBType) return Vkm_GenBType is
        (GBT.Make_GenType(left).Concatenate(right)) with Inline;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType concatenation operator.
    --<
    --< @description
    --< Concatenate a scalar Vkm_Bool and a Vkm_GenBType vector.
    --<    vector := vector & scalar;
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
    function "&" (left        : in     Vkm_GenBType;
                  right       : in     Vkm_Bool   ) return Vkm_GenBType is
        (left.Concatenate(GBT.Make_GenType(right))) with Inline;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType concatenation operator.
    --<
    --< @description
    --< Concatenate two scalar Vkm_Bool parameters.
    --<    vector := scalar & scalar;
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
    function "&" (left, right : in     Vkm_Bool   ) return Vkm_GenBType is
        (GBT.Make_GenType(left, right)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "not" operator.
    --<
    --< @description
    --< Apply a logical complement component-wise on a Vkm_GenBType vector.
    --<    vector := not vector
    ----------------------------------------------------------------------------
    function "not" is new GBT.Apply_Func_IV_RV("not");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "and" operator.
    --<
    --< @description
    --< Apply a logical "and" component-wise on two Vkm_GenBType vectors.
    --<     vector := vector and vector;
    ----------------------------------------------------------------------------
    function "and" is new GBT.Apply_Func_IV_IV_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "and" operator.
    --<
    --< @description
    --< Apply a logical "and" component-wise on a Vkm_GenBType vector and 
    --< Vkm_Bool scalar.
    --<     vector := vector and scalar;
    ----------------------------------------------------------------------------
    function "and" is new GBT.Apply_Func_IV_IS_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "and" operator.
    --<
    --< @description
    --< Apply a logical "and" component-wise on a Vkm_GenBType vector and 
    --< Vkm_Bool scalar.
    --<     vector := scalar and scalar;
    ----------------------------------------------------------------------------
    function "and" is new GBT.Apply_Func_IS_IV_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "or" operator.
    --<
    --< @description
    --< Apply a logical "or" component-wise on two Vkm_GenBType vectors.
    --<     vector := vector or vector;
    ----------------------------------------------------------------------------
    function "or" is new GBT.Apply_Func_IV_IV_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "or" operator.
    --<
    --< @description
    --< Apply a logical "or" component-wise on a Vkm_GenBType vector and 
    --< Vkm_Bool scalar.
    --<     vector := vector or scalar;
    ----------------------------------------------------------------------------
    function "or" is new GBT.Apply_Func_IV_IS_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "or" operator.
    --<
    --< @description
    --< Apply a logical "or" component-wise on a Vkm_GenBType vector and 
    --< Vkm_Bool scalar.
    --<     vector := scalar or vector;
    ----------------------------------------------------------------------------
    function "or" is new GBT.Apply_Func_IS_IV_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "xor" operator.
    --<
    --< @description
    --< Apply a logical "xor" component-wise on two Vkm_GenBType vectors.
    --<     vector := vector xor vector;
    ----------------------------------------------------------------------------
    function "xor" is new GBT.Apply_Func_IV_IV_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "xor" operator.
    --<
    --< @description
    --< Apply a logical "xor" component-wise on a Vkm_GenBType vector and 
    --< Vkm_Bool scalar.
    --<     vector := vector xor scalar;
    ----------------------------------------------------------------------------
    function "xor" is new GBT.Apply_Func_IV_IS_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType logical "xor" operator.
    --<
    --< @description
    --< Apply a logical "xor" component-wise on a Vkm_GenBType vector and 
    --< Vkm_Bool scalar.
    --<     vector := scalar xor vector;
    ----------------------------------------------------------------------------
    function "xor" is new GBT.Apply_Func_IS_IV_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType equality operator.
    --<
    --< @description
    --< Determine whether all components of two vectors are equal to each other.
    --<     is_equal := vector = vector;
    --<
    --< @param left
    --< The left parameter in the comparison.
    --<
    --< @param right
    --< The right parameter in the comparison.
    --<
    --< @return
    --< Whether the two vectors are equal to each other.
    ----------------------------------------------------------------------------
    function "="  (left, right : in     Vkm_GenBType) return Vkm_Bool is
        (Is_All(Equal(left,right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenBType inequality operator.
    --<
    --< @description
    --< Determine whether all components of two vectors are not equal to each other.
    --<     is_equal := vector /= vector;
    --<
    --< @param left
    --< The left parameter in the comparison.
    --<
    --< @param right
    --< The right parameter in the comparison.
    --<
    --< @return
    --< Whether the two vectors are not equal to each other.
    ----------------------------------------------------------------------------
    function "/=" (left, right : in     Vkm_GenBType) return Vkm_Bool is
        (Is_Any(Not_Equal(left, right))) with Inline;
    
    
    ----------------------------------------------------------------------------
    -- The Operators for unsigned integer vectors are defined here.
    --
    -- A summary of operators that can be used with GenUType values of different
    -- size:
    --    - "&", Concatenation
    --
    -- A summary of operators that are component-wise Unary:
    --    - "+"  , Unary plus operator.
    --    - "not", Bitwise complement operator
    --
    -- A summary of operators that are component-wise on two input vectors of the
    -- same length. Additionally, a scalar may appear instead of a vector on the
    -- left or right hand side of these operators:
    --    - "mod", Modulus operator.
    --    - "+",   Addition operator.
    --    - "-",   Subtraction operator.
    --    - "rem", Remainder operator.
    --    - "*",   Multiplication operator.
    --    - "/",   Division operator.
    --    - "and", Bitwise AND operator.
    --    - "or" , Bitwise OR operator.
    --    - "xor", Bitwise XOR operator.
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
    --< @summary
    --< Vkm_GenUType concatenation operator.
    --<
    --< @description
    --< Concatenate two Vkm_GenUType vectors.
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
    function "&" (left, right : in     Vkm_GenUType) return Vkm_GenUType renames GUT.Concatenate;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType concatenation operator.
    --<
    --< @description
    --< Concatenate a scalar Vkm_Uint and a Vkm_GenUType vector.
    --<    vector := scalar & vector;
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
    function "&" (left        : in     Vkm_Uint   ;
                  right       : in     Vkm_GenUType) return Vkm_GenUType is
        (GUT.Make_GenType(left).Concatenate(right)) with Inline;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType concatenation operator.
    --<
    --< @description
    --< Concatenate a scalar Vkm_Uint and a Vkm_GenUType vector.
    --<    vector := vector & scalar;
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
    function "&" (left        : in     Vkm_GenUType;
                  right       : in     Vkm_Uint   ) return Vkm_GenUType is
        (left.Concatenate(GUT.Make_GenType(right))) with Inline;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType concatenation operator.
    --<
    --< @description
    --< Concatenate a two scalars of Vkm_Uint type.
    --<    vector := scalar & scalar;
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
    function "&" (left, right : in     Vkm_Uint   ) return Vkm_GenUType is
        (GUT.Make_GenType(left, right)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType unary plus operator.
    --<
    --< @description
    --< Return the vector unmodified.
    --<    vector := +vector
    ----------------------------------------------------------------------------
    function "+" is new GUT.Apply_Func_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise complement operator.
    --<
    --< @description
    --< Apply the bitwise complement operator to each component of the vector,
    --< returning the resulting vector.
    --<    vector := not vector
    ----------------------------------------------------------------------------
    function "not" is new GUT.Apply_Func_IV_RV("not");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType modulus operator.
    --<
    --< @description
    --< Apply a modulus component-wise on two Vkm_GenBType vectors.
    --<     vector := vector mod vector;
    ----------------------------------------------------------------------------
    function "mod" is new GUT.Apply_Func_IV_IV_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType modulus operator.
    --<
    --< @description
    --< Apply a scalar modulus component-wise on a Vkm_GenUType vector.
    --<     vector := vector mod scalar;
    ----------------------------------------------------------------------------
    function "mod" is new GUT.Apply_Func_IV_IS_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType modulus operator.
    --<
    --< @description
    --< Apply a vector modulus component-wise on a Vkm_Uint scalar.
    --<     vector := scalar mod vector;
    ----------------------------------------------------------------------------
    function "mod" is new GUT.Apply_Func_IS_IV_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType addition operator.
    --<
    --< @description
    --< Apply a addition component-wise on two Vkm_GenUType vectors.
    --<     vector := vector + vector;
    ----------------------------------------------------------------------------
    function "+" is new GUT.Apply_Func_IV_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType addition operator.
    --<
    --< @description
    --< Add a scalar to each component of a vector.
    --<     vector := vector + scalar;
    ----------------------------------------------------------------------------
    function "+" is new GUT.Apply_Func_IV_IS_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType addition operator.
    --<
    --< @description
    --< Add a scalar to each component of a vector.
    --<     vector := scalar + vector;
    ----------------------------------------------------------------------------
    function "+" is new GUT.Apply_Func_IS_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType subtraction operator.
    --<
    --< @description
    --< Apply subtraction component-wise on two Vkm_GenUType vectors.
    --<     vector := vector - vector;
    ----------------------------------------------------------------------------
    function "-" is new GUT.Apply_Func_IV_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType subtraction operator.
    --<
    --< @description
    --< Subtract a scalar from each component of a vector.
    --<     vector := vector - scalar;
    ----------------------------------------------------------------------------
    function "-" is new GUT.Apply_Func_IV_IS_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType subtraction operator.
    --<
    --< @description
    --< Subtract each component of the vector from the scalar, returning the result
    --< for each subtraction in the corresponding component of a new vector.
    --<     vector := scalar - vector;
    ----------------------------------------------------------------------------
    function "-" is new GUT.Apply_Func_IS_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType remainder operator.
    --<
    --< @description
    --< Return the remainder from component-wise division of two Vkm_GenUType vectors.
    --<     vector := vector rem vector;
    ----------------------------------------------------------------------------
    function "rem" is new GUT.Apply_Func_IV_IV_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType remainder operator.
    --<
    --< @description
    --< Return the remainder from division of each component of a vector by a scalar.
    --<     vector := vector rem scalar;
    ----------------------------------------------------------------------------
    function "rem" is new GUT.Apply_Func_IV_IS_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType remainder operator.
    --<
    --< @description
    --< Return the remainder from division of a scalar by each component of a vector.
    --<     vector := scalar rem vector;
    ----------------------------------------------------------------------------
    function "rem" is new GUT.Apply_Func_IS_IV_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType mulitiplication operator.
    --<
    --< @description
    --< Return the result of component-wise multiplication between the two vectors.
    --<     vector := vector * vector;
    ----------------------------------------------------------------------------
    function "*" is new GUT.Apply_Func_IV_IV_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType mulitiplication operator.
    --<
    --< @description
    --< Return the result of component-wise multiplication of a vector and a scalar.
    --<     vector := vector * scalar;
    ----------------------------------------------------------------------------
    function "*" is new GUT.Apply_Func_IV_IS_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType mulitiplication operator.
    --<
    --< @description
    --< Return the result of component-wise multiplication of a vector and a scalar.
    --<     vector := scalar * vector;
    ----------------------------------------------------------------------------
    function "*" is new GUT.Apply_Func_IS_IV_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType division operator.
    --<
    --< @description
    --< Return the result of component-wise division between the two vectors.
    --<     vector := vector * vector;
    ----------------------------------------------------------------------------
    function "/" is new GUT.Apply_Func_IV_IV_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType division operator.
    --<
    --< @description
    --< Return the result of component-wise division between a vector and a scalar.
    --<     vector := vector / scalar;
    ----------------------------------------------------------------------------
    function "/" is new GUT.Apply_Func_IV_IS_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType division operator.
    --<
    --< @description
    --< Return the result of component-wise division between a vector and a scalar.
    --<     vector := scalar / vector;
    ----------------------------------------------------------------------------
    function "/" is new GUT.Apply_Func_IS_IV_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "and" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "and" between the two vectors.
    --<     vector := vector and vector;
    ----------------------------------------------------------------------------
    function "and" is new GUT.Apply_Func_IV_IV_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "and" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "and" between a scalar and a vector.
    --<     vector := vector and scalar;
    ----------------------------------------------------------------------------
    function "and" is new GUT.Apply_Func_IV_IS_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "and" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "and" between a scalar and a vector.
    --<     vector := scalar and vector;
    ----------------------------------------------------------------------------
    function "and" is new GUT.Apply_Func_IS_IV_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "or" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "or" between the two vectors.
    --<     vector := vector or vector;
    ----------------------------------------------------------------------------
    function "or" is new GUT.Apply_Func_IV_IV_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "or" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "or" between a scalar and a vector.
    --<     vector := vector or scalar;
    ----------------------------------------------------------------------------
    function "or" is new GUT.Apply_Func_IV_IS_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "or" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "or" between a scalar and a vector.
    --<     vector := scalar or vector;
    ----------------------------------------------------------------------------
    function "or" is new GUT.Apply_Func_IS_IV_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "xor" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "xor" between the two vectors.
    --<     vector := vector xor vector;
    ----------------------------------------------------------------------------
    function "xor" is new GUT.Apply_Func_IV_IV_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "xor" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "xor" between a vector and a scalar.
    --<     vector := vector xor scalar;
    ----------------------------------------------------------------------------
    function "xor" is new GUT.Apply_Func_IV_IS_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType bitwise "xor" operator.
    --<
    --< @description
    --< Return the result of component-wise bitwise "xor" between a vector and a scalar.
    --<     vector := scalar xor vector;
    ----------------------------------------------------------------------------
    function "xor" is new GUT.Apply_Func_IS_IV_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType less than operator.
    --<
    --< @description
    --< Return the result of the component-wise less than operator on two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function "<"  is new Apply_Func_IVU_IVU_RVB("<" );


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType less than or equal to operator.
    --<
    --< @description
    --< Return the result of the component-wise less than or equal operator on two vectors.
    --<     bool_vector := vector <= vector;
    ----------------------------------------------------------------------------
    function "<=" is new Apply_Func_IVU_IVU_RVB("<=");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType greater than operator.
    --<
    --< @description
    --< Return the result of the component-wise greater than operator on two vectors.
    --<     bool_vector := vector > vector;
    ----------------------------------------------------------------------------
    function ">"  is new Apply_Func_IVU_IVU_RVB(">" );


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType greater than or equal to operator.
    --<
    --< @description
    --< Return the result of the component-wise greater than or equal to operator on two vectors.
    --<     bool_vector := vector >= vector;
    ----------------------------------------------------------------------------
    function ">=" is new Apply_Func_IVU_IVU_RVB(">=");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType equality operator.
    --<
    --< @description
    --< Determine whether all components of two vectors are equal to each other.
    --<     is_equal := vector = vector;
    --<
    --< @param left
    --< The left parameter in the comparison.
    --<
    --< @param right
    --< The right parameter in the comparison.
    --<
    --< @return
    --< Whether the two vectors are equal to each other.
    ----------------------------------------------------------------------------
    function "="  (left, right : in     Vkm_GenUType) return Vkm_Bool is
        (Is_All(Equal(left,right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenUType inequality operator.
    --<
    --< @description
    --< Determine whether all components of two vectors are not equal to each other.
    --<     is_equal := vector /= vector;
    --<
    --< @param left
    --< The left parameter in the comparison.
    --<
    --< @param right
    --< The right parameter in the comparison.
    --<
    --< @return
    --< Whether the two vectors are not equal to each other.
    ----------------------------------------------------------------------------
    function "/=" (left, right : in     Vkm_GenUType) return Vkm_Bool is
        (Is_Any(Not_Equal(left, right))) with Inline;
    
    
    ----------------------------------------------------------------------------
    -- The Operators for integer vectors are defined here.
    --
    -- A summary of operators that can be used with GenIType values of different
    -- size:
    --    - "&", Concatenation
    --
    -- A summary of operators that are component-wise Unary:
    --    - "+"  , Unary plus operator.
    --    - "-"  , Unary minus operator.
    --    - "not", Bitwise complement operator
    --    - "abs", Unary absolute value operator.
    --
    -- A summary of operators that are component-wise on two input vectors of the
    -- same length. Additionally, a scalar may appear instead of a vector on the
    -- left or right hand side of these operators:
    --    - "mod", Modulus operator.
    --    - "+",   Addition operator.
    --    - "-",   Subtraction operator.
    --    - "rem", Remainder operator.
    --    - "*",   Multiplication operator.
    --    - "/",   Division operator.
    --    - "and", Bitwise AND operator.
    --    - "or" , Bitwise OR operator.
    --    - "xor", Bitwise XOR operator.
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
    -- GenIType Concatenation Operators
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType concatenation operator.
    --<
    --< @description
    --< Concatenate two Vkm_GenIType vectors.
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
    function "&" (left, right : in     Vkm_GenIType) return Vkm_GenIType renames GIT.Concatenate;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType concatenation operator.
    --<
    --< @description
    --< Concatenate a Vkm_GenIType vector and a Vkm_Int scalar.
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
    function "&" (left        : in     Vkm_Int   ;
                  right       : in     Vkm_GenIType) return Vkm_GenIType is
        (GIT.Make_GenType(left).Concatenate(right)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType concatenation operator.
    --<
    --< @description
    --< Concatenate a Vkm_GenIType vector and a Vkm_Int scalar.
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
    function "&" (left        : in     Vkm_GenIType;
                  right       : in     Vkm_Int   ) return Vkm_GenIType is
        (left.Concatenate(GIT.Make_GenType(right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType concatenation operator.
    --<
    --< @description
    --< Concatenate two Vkm_Int scalars.
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
    function "&" (left, right : in     Vkm_Int   ) return Vkm_GenIType is
        (GIT.Make_GenType(left, right)) with Inline;

    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType unary plus operator.
    --<
    --< @description
    --< Return the vector unmodified.
    --<    vector := +vector
    ----------------------------------------------------------------------------
    function "+" is new GIT.Apply_Func_IV_RV("+");

    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType unary minus operator.
    --<
    --< @description
    --< Return the vector negated.
    --<    vector := -vector
    ----------------------------------------------------------------------------
    function "-" is new GIT.Apply_Func_IV_RV("-");

    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_Int bitwise complement operator.
    --<
    --< @description
    --< Return the 1's complement for the Vkm_Int value.
    --< 
    --< @param value
    --< The value to perform the 1's complement of.
    --<
    --< @return
    --< The 1's complement of the value.
    ----------------------------------------------------------------------------
    function "not" (value : in     Vkm_Int) return Vkm_Int is
        (To_Vkm_Int(not To_Vkm_Uint(value))) with Inline;

    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise complement operator.
    --<
    --< @description
    --< Return the 1's complement for each component of the Vkm_GenIType vector.
    ----------------------------------------------------------------------------
    function "not" is new GIT.Apply_Func_IV_RV("not");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType unary absolute value operator.
    --<
    --< @description
    --< Return the absolute value for each component of the vector.
    --<     vector := abs vector
    ----------------------------------------------------------------------------
    function "abs" is new GIT.Apply_Func_IV_RV("abs");
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType modulus operator.
    --<
    --< @description
    --< Apply a modulus component-wise on two Vkm_GenIType vectors.
    --<     vector := vector mod vector;
    ----------------------------------------------------------------------------
    function "mod" is new GIT.Apply_Func_IV_IV_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType modulus operator.
    --<
    --< @description
    --< Apply a scalar modulus component-wise on a vector.
    --<     vector := vector mod scalar;
    ----------------------------------------------------------------------------
    function "mod" is new GIT.Apply_Func_IV_IS_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType modulus operator.
    --<
    --< @description
    --< Apply a vector modulus component-wise on a scalar.
    --<     vector := scalar mod vector;
    ----------------------------------------------------------------------------
    function "mod" is new GIT.Apply_Func_IS_IV_RV("mod");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType addition operator.
    --<
    --< @description
    --< Apply addition component-wise on two vectors.
    --<     vector := vector + vector;
    ----------------------------------------------------------------------------
    function "+" is new GIT.Apply_Func_IV_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType addition operator.
    --<
    --< @description
    --< Add a scalar to each component of a vector.
    --<     vector := vector + scalar;
    ----------------------------------------------------------------------------
    function "+" is new GIT.Apply_Func_IV_IS_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType addition operator.
    --<
    --< @description
    --< Add a scalar to each component of a vector.
    --<     vector := scalar + vector;
    ----------------------------------------------------------------------------
    function "+" is new GIT.Apply_Func_IS_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType subtraction operator.
    --<
    --< @description
    --< Subtract a vector from another vector.
    --<     vector := vector - vector;
    ----------------------------------------------------------------------------
    function "-" is new GIT.Apply_Func_IV_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType subtraction operator.
    --<
    --< @description
    --< Subtract a scalar from each component of a vector.
    --<     vector := vector - scalar;
    ----------------------------------------------------------------------------
    function "-" is new GIT.Apply_Func_IV_IS_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType subtraction operator.
    --<
    --< @description
    --< Subtract each component of a vector from a scalar, returning a vector of
    --< the result.
    --<     vector := scalar - vector;
    ----------------------------------------------------------------------------
    function "-" is new GIT.Apply_Func_IS_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType remainder operator.
    --<
    --< @description
    --< Return the remainder from component-wise division of two Vkm_GenIType vectors.
    --<     vector := vector rem vector;
    ----------------------------------------------------------------------------
    function "rem" is new GIT.Apply_Func_IV_IV_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType remainder operator.
    --<
    --< @description
    --< Return the remainder from division of a vector by a scalar.
    --<     vector := vector rem scalar;
    ----------------------------------------------------------------------------
    function "rem" is new GIT.Apply_Func_IV_IS_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType remainder operator.
    --<
    --< @description
    --< Return the remainder from division of a scalar by a vector.
    --<     vector := scalar rem vector;
    ----------------------------------------------------------------------------
    function "rem" is new GIT.Apply_Func_IS_IV_RV("rem");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType multiplication operator.
    --<
    --< @description
    --< Apply multiplication component-wise on two vectors.
    --<     vector := vector * vector;
    ----------------------------------------------------------------------------
    function "*" is new GIT.Apply_Func_IV_IV_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType multiplication operator.
    --<
    --< @description
    --< Multiply each component of a vector by a scalar.
    --<     vector := vector * scalar;
    ----------------------------------------------------------------------------
    function "*" is new GIT.Apply_Func_IV_IS_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType multiplication operator.
    --<
    --< @description
    --< Multiply each component of a vector by a scalar.
    --<     vector := scalar * vector;
    ----------------------------------------------------------------------------
    function "*" is new GIT.Apply_Func_IS_IV_RV("*");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType division operator.
    --<
    --< @description
    --< Apply division component-wise on two vectors.
    --<     vector := vector / vector;
    ----------------------------------------------------------------------------
    function "/" is new GIT.Apply_Func_IV_IV_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType division operator.
    --<
    --< @description
    --< Apply divide components of a vector by a scalar.
    --<     vector := vector / scalar;
    ----------------------------------------------------------------------------
    function "/" is new GIT.Apply_Func_IV_IS_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType division operator.
    --<
    --< @description
    --< Divide a scalar by components of a vector.
    --<     vector := scalar / vector;
    ----------------------------------------------------------------------------
    function "/" is new GIT.Apply_Func_IS_IV_RV("/");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_Int bitwise "and" operator.
    --<
    --< @description
    --< Return the result of the bitwise "and" for to scalar values.
    --<     scalar := scalar and scalar;
    --<
    --< @param left
    --< The parameter to the left of the "and" keyword.
    --< 
    --< @param right
    --< The parameter to the right of the "and" keyword.
    --<
    --< @return
    --< The bitwise and of left with right.
    ----------------------------------------------------------------------------
    function "and" (left, right : in     Vkm_Int) return Vkm_Int is
        (To_Vkm_Int( To_Vkm_Uint(left) and To_Vkm_Uint(right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "and" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "and" for two vectors.
    --<     vector := vector and vector;
    ----------------------------------------------------------------------------
    function "and" is new GIT.Apply_Func_IV_IV_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "and" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "and" for a vector and a scalar.
    --<     vector := vector and scalar;
    ----------------------------------------------------------------------------
    function "and" is new GIT.Apply_Func_IV_IS_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "and" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "and" for a vector and a scalar.
    --<     vector := scalar and vector;
    ----------------------------------------------------------------------------
    function "and" is new GIT.Apply_Func_IS_IV_RV("and");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_Int bitwise "or" operator.
    --<
    --< @description
    --< Return the result of the bitwise "or" for to scalar values.
    --<     scalar := scalar or scalar;
    --< 
    --< @param left
    --< The parameter to the left of the "or" keyword.
    --<
    --< @param right
    --< The parameter to the right of the "or" keyword.
    --<
    --< @return
    --< The result of the bitwise "or" operation.
    ----------------------------------------------------------------------------
    function "or" (left, right : in     Vkm_Int) return Vkm_Int is
        (To_Vkm_Int(To_Vkm_Uint(left) or To_Vkm_Uint(right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "or" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "or" for two vectors.
    --<     vector := vector or vector;
    ----------------------------------------------------------------------------
    function "or" is new GIT.Apply_Func_IV_IV_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "or" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "or" for a vector and a scalar.
    --<     vector := vector or scalar;
    ----------------------------------------------------------------------------
    function "or" is new GIT.Apply_Func_IV_IS_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "or" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "or" for a vector and a scalar.
    --<     vector := scalar or vector;
    ----------------------------------------------------------------------------
    function "or" is new GIT.Apply_Func_IS_IV_RV("or");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_Int bitwise "xor" operator.
    --<
    --< @description
    --< Return the result of the bitwise "xor" for to scalar values.
    --<     scalar := scalar xor scalar;
    --<
    --< @param left
    --< The parameter to the left of the "xor" keyword.
    --<
    --< @param right
    --< The parameter to the right of the "xor" keyword.
    --<
    --< @return
    --< The result of the bitwise "xor" operation.
    ----------------------------------------------------------------------------
    function "xor" (left, right : in     Vkm_Int) return Vkm_Int is
        (To_Vkm_Int(To_Vkm_Uint(left) xor To_Vkm_Uint(right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "xor" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "xor" for two vectors.
    --<     vector := vector xor vector;
    ----------------------------------------------------------------------------
    function "xor" is new GIT.Apply_Func_IV_IV_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "xor" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "xor" for a vector and a scalar.
    --<     vector := vector xor scalar;
    ----------------------------------------------------------------------------
    function "xor" is new GIT.Apply_Func_IV_IS_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType bitwise "xor" operator.
    --<
    --< @description
    --< Return the component-wise bitwise "xor" for a vector and a scalar.
    --<     vector := scalar xor vector;
    ----------------------------------------------------------------------------
    function "xor" is new GIT.Apply_Func_IS_IV_RV("xor");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType less than operator.
    --<
    --< @description
    --< Return the result of the component-wise less than operator on two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function "<"  is new Apply_Func_IVI_IVI_RVB("<" );


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType less than or equal to operator.
    --<
    --< @description
    --< Return the result of the component-wise less than or equal to operator on 
    --< two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function "<=" is new Apply_Func_IVI_IVI_RVB("<=");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType greater than operator.
    --<
    --< @description
    --< Return the result of the component-wise greater than operator on 
    --< two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function ">"  is new Apply_Func_IVI_IVI_RVB(">" );


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType greater than or equal operator.
    --<
    --< @description
    --< Return the result of the component-wise greater than or equal to operator 
    --< on two vectors.
    --<     bool_vector := vector < vector;
    ----------------------------------------------------------------------------
    function ">=" is new Apply_Func_IVI_IVI_RVB(">=");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType equality operator.
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
    function "="  (left, right : in     Vkm_GenIType) return Vkm_Bool is
        (Is_All(Equal(left, right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenIType inequality operator.
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
    function "/=" (left, right : in     Vkm_GenIType) return Vkm_Bool is
        (Is_Any(Not_Equal(left, right))) with Inline;
    
    
    ----------------------------------------------------------------------------
    -- The Operators for single precision floating point vectors are defined here.
    --
    -- A summary of operators that can be used with GenFType values of different
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
    -- GenFType Concatenation Operators
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType concatenation operator.
    --<
    --< @description
    --< Concatenate two Vkm_GenFType vectors.
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
    function "&" (left, right : in     Vkm_GenFType) return Vkm_GenFType renames GFT.Concatenate;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType concatenation operator.
    --<
    --< @description
    --< Concatenate a Vkm_Float and a Vkm_GenFType vector.
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
    function "&" (left        : in     Vkm_Float   ;
                  right       : in     Vkm_GenFType) return Vkm_GenFType is
        (GFT.Make_GenType(left).Concatenate(right)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType concatenation operator.
    --<
    --< @description
    --< Concatenate a Vkm_Float and a Vkm_GenFType vector.
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
    function "&" (left        : in     Vkm_GenFType;
                  right       : in     Vkm_Float   ) return Vkm_GenFType is
        (left.Concatenate(GFT.Make_GenType(right))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType concatenation operator.
    --<
    --< @description
    --< Concatenate two Vkm_Floats to form a Vkm_GenFType vector.
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
    function "&" (left, right : in     Vkm_Float   ) return Vkm_GenFType is
        (GFT.Make_GenType(left, right)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType unary plus operator.
    --<
    --< @description
    --< Return the unmodified vector.
    --<     vector :=  +vector;
    ----------------------------------------------------------------------------
    function "+" is new GFT.Apply_Func_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType unary minus operator.
    --<
    --< @description
    --< Return the negation of the vector.
    --<     vector :=  -vector;
    ----------------------------------------------------------------------------
    function "-" is new GFT.Apply_Func_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType absolute value operator.
    --<
    --< @description
    --< Return the absolute value for each component of the vector.
    --<     vector :=  abs vector;
    ----------------------------------------------------------------------------
    function "abs" is new GFT.Apply_Func_IV_RV(Vulkan.Math.Common.Absolute_Value);


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType modulo operator.
    --<
    --< @description
    --< Return the component-wise modulo of the two vectors.
    --<     vector :=  vector mod vector;
    ----------------------------------------------------------------------------
    function "mod" is new GFT.Apply_Func_IV_IV_RV(Vulkan.Math.Common.Modulo);


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType modulo operator.
    --<
    --< @description
    --< Return the component-wise modulo of a vector and a scalar.
    --<     vector :=  vector mod scalar;
    ----------------------------------------------------------------------------
    function "mod" is new GFT.Apply_Func_IV_IS_RV(Vulkan.Math.Common.Modulo);


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType modulo operator.
    --<
    --< @description
    --< Return the component-wise modulo of a vector and a scalar.
    --<     vector :=  scalar mod vector;
    ----------------------------------------------------------------------------
    function "mod" is new GFT.Apply_Func_IS_IV_RV(Vulkan.Math.Common.Modulo);


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_Float power operator.
    --<
    --< @description
    --< Take the left float to the right power.
    --<    scalar := left ** right;
    --<
    --< @param left
    --< The parameter to the left of the "**", power, operator.
    --<
    --< @param right
    --< The parameter to the right of the "**", power, operator.
    --<
    --< @return
    --< The left value taken to the right power.
    ----------------------------------------------------------------------------
    function "**" (left, right : in     Vkm_Float) return Vkm_Float renames Vulkan.Math.Exp.Pow;


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType power operator.
    --<
    --< @description
    --< Apply the power operation component-wise on two vectors.
    --<    vector := vector ** vector;
    ----------------------------------------------------------------------------
    function "**" is new GFT.Apply_Func_IV_IV_RV(Vulkan.Math.Exp.Pow);


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType power operator.
    --<
    --< @description
    --< Apply the power operation component-wise on a vector and a scalar.
    --<    vector := vector ** scalar;
    ----------------------------------------------------------------------------
    function "**" is new GFT.Apply_Func_IV_IS_RV(Vulkan.Math.Exp.Pow);


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType power operator.
    --<
    --< @description
    --< Apply the power operation component-wise on a vector and a scalar.
    --<    vector := scalar ** vector;
    ----------------------------------------------------------------------------
    function "**" is new GFT.Apply_Func_IS_IV_RV(Vulkan.Math.Exp.Pow);


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType addition operator.
    --<
    --< @description
    --< Apply the additon operation component-wise on two vectors.
    --<    vector := vector + vector;
    ----------------------------------------------------------------------------
    function "+" is new GFT.Apply_Func_IV_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType addition operator.
    --<
    --< @description
    --< Apply the additon operation component-wise on a vector and a scalar.
    --<    vector := vector + scalar;
    ----------------------------------------------------------------------------
    function "+" is new GFT.Apply_Func_IV_IS_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType addition operator.
    --<
    --< @description
    --< Apply the additon operation component-wise on a vector and a scalar.
    --<    vector := scalar + vector;
    ----------------------------------------------------------------------------
    function "+" is new GFT.Apply_Func_IS_IV_RV("+");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType subtraction operator.
    --<
    --< @description
    --< Apply the subtraction operation component-wise on two vectors.
    --<    vector := vector - vector;
    ----------------------------------------------------------------------------
    function "-" is new GFT.Apply_Func_IV_IV_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType subtraction operator.
    --<
    --< @description
    --< Apply the subtraction operation component-wise on a vector and a scalar.
    --<    vector := vector - scalar;
    ----------------------------------------------------------------------------
    function "-" is new GFT.Apply_Func_IV_IS_RV("-");


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenFType subtraction operator.
    --<
    --< @description
    --< Apply the subtraction operation component-wise on a vector and a scalar.
    --<    vector := scalar - vector;
    ----------------------------------------------------------------------------
    function "-" is new GFT.Apply_Func_IS_IV_RV("-");

    ----------------------------------------------------------------------------
    -- GenFType Remainder Operator
    ----------------------------------------------------------------------------
    function "rem" (left, right : in     Vkm_Float) return Vkm_Float renames Vkm_Float'Remainder;
    function "rem" is new GFT.Apply_Func_IV_IV_RV("rem"); -- vector := vector rem vector
    function "rem" is new GFT.Apply_Func_IV_IS_RV("rem"); -- vector := vector rem scalar
    function "rem" is new GFT.Apply_Func_IS_IV_RV("rem"); -- vector := scalar rem vector

    ----------------------------------------------------------------------------
    -- GenFType Multiplication Operator
    ----------------------------------------------------------------------------
    function "*" is new GFT.Apply_Func_IV_IV_RV("*"); -- vector := vector * vector
    function "*" is new GFT.Apply_Func_IV_IS_RV("*"); -- vector := vector * scalar
    function "*" is new GFT.Apply_Func_IS_IV_RV("*"); -- vector := scalar * vector

    ----------------------------------------------------------------------------
    -- GenFType Division Operator
    ----------------------------------------------------------------------------
    function "/" is new GFT.Apply_Func_IV_IV_RV("/"); -- vector := vector / vector
    function "/" is new GFT.Apply_Func_IV_IS_RV("/"); -- vector := vector / scalar
    function "/" is new GFT.Apply_Func_IS_IV_RV("/"); -- vector := scalar / vector


    ----------------------------------------------------------------------------
    -- GenFType Relational Operators
    ----------------------------------------------------------------------------
    function "<"  is new Apply_Func_IVF_IVF_RVB("<" );
    function "<=" is new Apply_Func_IVF_IVF_RVB("<=");
    function ">"  is new Apply_Func_IVF_IVF_RVB(">" );
    function ">=" is new Apply_Func_IVF_IVF_RVB(">=");
    function "="  is new Apply_Func_IVF_IVF_RVB("=" );
    function "/=" is new Apply_Func_IVF_IVF_RVB("/=");


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
    --    - "=",  Equality operator
    --    - "/=", Non-Equality operator (Implicitly defined)
    --
    ----------------------------------------------------------------------------
    -- GenDType Concatenation Operators
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenDType concatenation operator.
    --<
    --< @description
    --< Concatenate two Vkm_GenDType vectors.
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
    -- GenDType Unary Plus Operator
    ----------------------------------------------------------------------------
    function "+" is new GDT.Apply_Func_IV_RV("+");

    ----------------------------------------------------------------------------
    -- GenDType Unary Minus Operator
    ----------------------------------------------------------------------------
    function "-" is new GDT.Apply_Func_IV_RV("-");

    ----------------------------------------------------------------------------
    -- GenDType Absolute Value Operator
    ----------------------------------------------------------------------------
    function "abs" is new GDT.Apply_Func_IV_RV(Vulkan.Math.Common.Absolute_Value); -- vector := abs vec

    ----------------------------------------------------------------------------
    -- GenDType Modulus Operator
    ----------------------------------------------------------------------------
    function "mod" is new GDT.Apply_Func_IV_IV_RV(Vulkan.Math.Common.Modulo); -- vector := vector mod vector
    function "mod" is new GDT.Apply_Func_IV_IS_RV(Vulkan.Math.Common.Modulo); -- vector := vector mod scalar
    function "mod" is new GDT.Apply_Func_IS_IV_RV(Vulkan.Math.Common.Modulo); -- vector := scalar mod vector

    ----------------------------------------------------------------------------
    -- GenDType Power Operator
    ----------------------------------------------------------------------------
    function "**" (left, right : in     Vkm_Double) return Vkm_Double renames Vulkan.Math.Exp.Pow;
    function "**" is new GDT.Apply_Func_IV_IV_RV(Vulkan.Math.Exp.Pow); -- vector := vector ** vector
    function "**" is new GDT.Apply_Func_IV_IS_RV(Vulkan.Math.Exp.Pow); -- vector := vector ** scalar
    function "**" is new GDT.Apply_Func_IS_IV_RV(Vulkan.Math.Exp.Pow); -- vector := scalar ** vector

    ----------------------------------------------------------------------------
    -- GenDType Addition Operator
    ----------------------------------------------------------------------------
    function "+" is new GDT.Apply_Func_IV_IV_RV("+"); -- vector := vector + vector
    function "+" is new GDT.Apply_Func_IV_IS_RV("+"); -- vector := vector + scalar
    function "+" is new GDT.Apply_Func_IS_IV_RV("+"); -- vector := scalar + vector

    ----------------------------------------------------------------------------
    -- GenDType Subtraction Operator
    ----------------------------------------------------------------------------
    function "-" is new GDT.Apply_Func_IV_IV_RV("-"); -- vector := vector - vector
    function "-" is new GDT.Apply_Func_IV_IS_RV("-"); -- vector := vector - scalar
    function "-" is new GDT.Apply_Func_IS_IV_RV("-"); -- vector := scalar - vector

    ----------------------------------------------------------------------------
    -- GenDType Remainder Operator
    ----------------------------------------------------------------------------
    function "rem" (left, right : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Remainder;
    function "rem" is new GDT.Apply_Func_IV_IV_RV("rem"); -- vector := vector rem vector
    function "rem" is new GDT.Apply_Func_IV_IS_RV("rem"); -- vector := vector rem scalar
    function "rem" is new GDT.Apply_Func_IS_IV_RV("rem"); -- vector := scalar rem vector

    ----------------------------------------------------------------------------
    -- GenDType Multiplication Operator
    ----------------------------------------------------------------------------
    function "*" is new GDT.Apply_Func_IV_IV_RV("*"); -- vector := vector * vector
    function "*" is new GDT.Apply_Func_IV_IS_RV("*"); -- vector := vector * scalar
    function "*" is new GDT.Apply_Func_IS_IV_RV("*"); -- vector := scalar * vector

    ----------------------------------------------------------------------------
    -- GenDType Division Operator
    ----------------------------------------------------------------------------
    function "/" is new GDT.Apply_Func_IV_IV_RV("/"); -- vector := vector / vector
    function "/" is new GDT.Apply_Func_IV_IS_RV("/"); -- vector := vector / scalar
    function "/" is new GDT.Apply_Func_IS_IV_RV("/"); -- vector := scalar / vector


    ----------------------------------------------------------------------------
    -- GenDType Relational Operators
    ----------------------------------------------------------------------------
    function "<"  is new Apply_Func_IVD_IVD_RVB("<");
    function "<=" is new Apply_Func_IVD_IVD_RVB("<=");
    function ">"  is new Apply_Func_IVD_IVD_RVB(">");
    function ">=" is new Apply_Func_IVD_IVD_RVB(">=");
    function "="  is new Apply_Func_IVD_IVD_RVB("=");
    function "/=" is new Apply_Func_IVD_IVD_RVB("/=");

end Vulkan.Math.Operators;
