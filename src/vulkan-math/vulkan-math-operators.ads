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
--
-- This package provides operator definitions for all types.
--------------------------------------------------------------------------------
with Vulkan.Math.GenFType;
with Vulkan.Math.GenDType;
with Vulkan.Math.GenUType;
with Vulkan.Math.Exp;
with Vulkan.Math.Common;

use Vulkan.Math.GenFType;
use Vulkan.Math.GenDType;
use Vulkan.Math.GenUType;

package Vulkan.Math.Operators is
    pragma Preelaborate;
    pragma Pure;


    ----------------------------------------------------------------------------
    -- The Operators for unsigned integer vectors are defined here.
    --
    -- A summary of operators that can be used with GenUType values of different
    -- size:
    --    - "&", Concatenation
    --
    -- A summary of operators that are component-wise Unary:
    --    - "+"  , Unary plus operator.
    --    - "-"  , Unary minus operator.
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
    --    - "=",  Equality operator
    --    - "/=", Non-Equality operator (Implicitly defined)
    --
    ----------------------------------------------------------------------------
    -- GenUType Concatenation Operators
    ----------------------------------------------------------------------------
    function "&" (Left, Right : in     Vkm_GenUType) return Vkm_GenUType renames GUT.Concatenate;
    function "&" (Left        : in     Vkm_Uint   ;
                  Right       : in     Vkm_GenUType) return Vkm_GenUType is
        (GUT.Make(Left).Concatenate(Right)) with Inline;
    function "&" (Left        : in     Vkm_GenUType;
                  Right       : in     Vkm_Uint   ) return Vkm_GenUType is
        (Left.Concatenate(GUT.Make(Right))) with Inline;
    function "&" (Left, Right : in     Vkm_Uint   ) return Vkm_GenUType is
        (GUT.Make(Left, Right)) with Inline;

    ----------------------------------------------------------------------------
    -- GenUType Unary Plus Operator
    ----------------------------------------------------------------------------
    function "+" is new GUT.Apply_Func_IV_RV("+");

    ----------------------------------------------------------------------------
    -- GenUType Unary Minus Operator
    ----------------------------------------------------------------------------
    function "-" is new GUT.Apply_Func_IV_RV("-");


    ----------------------------------------------------------------------------
    -- GenUType Unary Complement Operator
    ----------------------------------------------------------------------------
    function "not" is new GUT.Apply_Func_IV_RV("not");
    
    ----------------------------------------------------------------------------
    -- GenUType Modulus Operator
    ----------------------------------------------------------------------------
    function "mod" is new GUT.Apply_Func_IV_IV_RV("mod"); -- vector := vector mod vector
    function "mod" is new GUT.Apply_Func_IV_IS_RV("mod"); -- vector := vector mod scalar
    function "mod" is new GUT.Apply_Func_IS_IV_RV("mod"); -- vector := scalar mod vector

    ----------------------------------------------------------------------------
    -- GenUType Addition Operator
    ----------------------------------------------------------------------------
    function "+" is new GUT.Apply_Func_IV_IV_RV("+"); -- vector := vector + vector
    function "+" is new GUT.Apply_Func_IV_IS_RV("+"); -- vector := vector + scalar
    function "+" is new GUT.Apply_Func_IS_IV_RV("+"); -- vector := scalar + vector

    ----------------------------------------------------------------------------
    -- GenUType Subtraction Operator
    ----------------------------------------------------------------------------
    function "-" is new GUT.Apply_Func_IV_IV_RV("-"); -- vector := vector - vector
    function "-" is new GUT.Apply_Func_IV_IS_RV("-"); -- vector := vector - scalar
    function "-" is new GUT.Apply_Func_IS_IV_RV("-"); -- vector := scalar - vector

    ----------------------------------------------------------------------------
    -- GenUType Remainder Operator
    ----------------------------------------------------------------------------
    function "rem" is new GUT.Apply_Func_IV_IV_RV("rem"); -- vector := vector rem vector
    function "rem" is new GUT.Apply_Func_IV_IS_RV("rem"); -- vector := vector rem scalar
    function "rem" is new GUT.Apply_Func_IS_IV_RV("rem"); -- vector := scalar rem vector

    ----------------------------------------------------------------------------
    -- GenUType Multiplication Operator
    ----------------------------------------------------------------------------
    function "*" is new GUT.Apply_Func_IV_IV_RV("*"); -- vector := vector * vector
    function "*" is new GUT.Apply_Func_IV_IS_RV("*"); -- vector := vector * scalar
    function "*" is new GUT.Apply_Func_IS_IV_RV("*"); -- vector := scalar * vector

    ----------------------------------------------------------------------------
    -- GenUType Division Operator
    ----------------------------------------------------------------------------
    function "/" is new GUT.Apply_Func_IV_IV_RV("/"); -- vector := vector / vector
    function "/" is new GUT.Apply_Func_IV_IS_RV("/"); -- vector := vector / scalar
    function "/" is new GUT.Apply_Func_IS_IV_RV("/"); -- vector := scalar / vector

    ----------------------------------------------------------------------------
    -- GenUType Bitwise AND Operator
    ----------------------------------------------------------------------------
    function "and" is new GUT.Apply_Func_IV_IV_RV("and"); -- vector := vector and vector
    function "and" is new GUT.Apply_Func_IV_IS_RV("and"); -- vector := vector and scalar
    function "and" is new GUT.Apply_Func_IS_IV_RV("and"); -- vector := scalar and vector

    ----------------------------------------------------------------------------
    -- GenUType Bitwise OR Operator
    ----------------------------------------------------------------------------
    function "or" is new GUT.Apply_Func_IV_IV_RV("or"); -- vector := vector or vector
    function "or" is new GUT.Apply_Func_IV_IS_RV("or"); -- vector := vector or scalar
    function "or" is new GUT.Apply_Func_IS_IV_RV("or"); -- vector := scalar or vector

    ----------------------------------------------------------------------------
    -- GenUType Bitwise XOR Operator
    ----------------------------------------------------------------------------
    function "xor" is new GUT.Apply_Func_IV_IV_RV("xor"); -- vector := vector xor vector
    function "xor" is new GUT.Apply_Func_IV_IS_RV("xor"); -- vector := vector xor scalar
    function "xor" is new GUT.Apply_Func_IS_IV_RV("xor"); -- vector := scalar xor vector

    ----------------------------------------------------------------------------
    -- GenUType Relational Operators
    ----------------------------------------------------------------------------
    function "<"  is new Apply_Func_IVU_IVU_RVB("<" );
    function "<=" is new Apply_Func_IVU_IVU_RVB("<=");
    function ">"  is new Apply_Func_IVU_IVU_RVB(">" );
    function ">=" is new Apply_Func_IVU_IVU_RVB(">=");
    function "="  is new Apply_Func_IVU_IVU_RVB("=" );
    function "/=" is new Apply_Func_IVU_IVU_RVB("/=");
    
    
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
    --    - "=",  Equality operator
    --    - "/=", Non-Equality operator (Implicitly defined)
    --
    ----------------------------------------------------------------------------
    -- GenFType Concatenation Operators
    ----------------------------------------------------------------------------
    function "&" (Left, Right : in     Vkm_GenFType) return Vkm_GenFType renames GFT.Concatenate;
    function "&" (Left        : in     Vkm_Float   ;
                  Right       : in     Vkm_GenFType) return Vkm_GenFType is
        (GFT.Make(Left).Concatenate(Right)) with Inline;
    function "&" (Left        : in     Vkm_GenFType;
                  Right       : in     Vkm_Float   ) return Vkm_GenFType is
        (Left.Concatenate(GFT.Make(Right))) with Inline;
    function "&" (Left, Right : in     Vkm_Float   ) return Vkm_GenFType is
        (GFT.Make(Left, Right)) with Inline;

    ----------------------------------------------------------------------------
    -- GenFtype Unary Plus Operator
    ----------------------------------------------------------------------------
    function "+" is new GFT.Apply_Func_IV_RV("+");

    ----------------------------------------------------------------------------
    -- GenFType Unary Minus Operator
    ----------------------------------------------------------------------------
    function "-" is new GFT.Apply_Func_IV_RV("-");

    ----------------------------------------------------------------------------
    -- GenFType Absolute Value Operator
    ----------------------------------------------------------------------------
    function "abs" is new GFT.Apply_Func_IV_RV(Vulkan.Math.Common.Absolute_Value); -- vector := abs vec

    ----------------------------------------------------------------------------
    -- GenFType Modulus Operator
    ----------------------------------------------------------------------------
    function "mod" is new GFT.Apply_Func_IV_IV_RV(Vulkan.Math.Common.Modulo); -- vector := vector mod vector
    function "mod" is new GFT.Apply_Func_IV_IS_RV(Vulkan.Math.Common.Modulo); -- vector := vector mod scalar
    function "mod" is new GFT.Apply_Func_IS_IV_RV(Vulkan.Math.Common.Modulo); -- vector := scalar mod vector

    ----------------------------------------------------------------------------
    -- GenFType Power Operator
    ----------------------------------------------------------------------------
    function "**" (Left, Right : in     Vkm_Float) return Vkm_Float renames Vulkan.Math.Exp.Pow;
    function "**" is new GFT.Apply_Func_IV_IV_RV(Vulkan.Math.Exp.Pow); -- vector := vector ** vector
    function "**" is new GFT.Apply_Func_IV_IS_RV(Vulkan.Math.Exp.Pow); -- vector := vector ** scalar
    function "**" is new GFT.Apply_Func_IS_IV_RV(Vulkan.Math.Exp.Pow); -- vector := scalar ** vector

    ----------------------------------------------------------------------------
    -- GenFType Addition Operator
    ----------------------------------------------------------------------------
    function "+" is new GFT.Apply_Func_IV_IV_RV("+"); -- vector := vector + vector
    function "+" is new GFT.Apply_Func_IV_IS_RV("+"); -- vector := vector + scalar
    function "+" is new GFT.Apply_Func_IS_IV_RV("+"); -- vector := scalar + vector

    ----------------------------------------------------------------------------
    -- GenFType Subtraction Operator
    ----------------------------------------------------------------------------
    function "-" is new GFT.Apply_Func_IV_IV_RV("-"); -- vector := vector - vector
    function "-" is new GFT.Apply_Func_IV_IS_RV("-"); -- vector := vector - scalar
    function "-" is new GFT.Apply_Func_IS_IV_RV("-"); -- vector := scalar - vector

    ----------------------------------------------------------------------------
    -- GenFType Remainder Operator
    ----------------------------------------------------------------------------
    function "rem" (Left, Right : in     Vkm_Float) return Vkm_Float renames Vkm_Float'Remainder;
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
    function "&" (Left, Right : in     Vkm_GenDType) return Vkm_GenDType renames GDT.Concatenate;
    function "&" (Left        : in     Vkm_Double   ;
                  Right       : in     Vkm_GenDType) return Vkm_GenDType is
        (GDT.Make(Left).Concatenate(Right)) with Inline;
    function "&" (Left        : in     Vkm_GenDType;
                  Right       : in     Vkm_Double   ) return Vkm_GenDType is
        (Left.Concatenate(GDT.Make(Right))) with Inline;
    function "&" (Left, Right : in     Vkm_Double   ) return Vkm_GenDType is
        (GDT.Make(Left, Right)) with Inline;

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
    function "**" (Left, Right : in     Vkm_Double) return Vkm_Double renames Vulkan.Math.Exp.Pow;
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
    function "rem" (Left, Right : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Remainder;
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
