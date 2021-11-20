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
with Interfaces.C;
with Ada.Numerics;
with Ada.Unchecked_Conversion;
with Ada.Numerics.Generic_Elementary_Functions;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
package Vulkan.Math is
    pragma Preelaborate;
    pragma Pure;


    ----------------------------------------------------------------------------
    -- Math Constants
    ----------------------------------------------------------------------------
    --< A constant value representing PI.
    PI : constant := Ada.Numerics.Pi;

    --< A constant value representing Euler's number e.
    E  : constant := Ada.Numerics.e;

    --< The constant natural logarithm of 2 value. This constant is used in the
    --< implementation of Exp2().
    LN2 : constant := 0.69314_71805_59945_30941_72321_21458_18;

    ----------------------------------------------------------------------------
    -- Math Scalar Types
    ----------------------------------------------------------------------------
    --< A value that can either be true or false. This type has the same size
    --< as a boolean value in C.
    type Vkm_Bool is new Boolean;
    for Vkm_Bool use (False => 0,
                      True  => 1);
    for Vkm_Bool'Size use Interfaces.C.unsigned_char'Size;

    --< A 32-bit unsigned integer type.
    type Vkm_Uint is new Interfaces.C.unsigned;

    --< A 32-bit 2's complement signed integer type.
    type Vkm_Int is new Interfaces.C.int;

    --< A 32-bit single precision signed floating point number.
    type Vkm_Float is new Interfaces.C.C_Float;

    --< A 64-bit double precision signed floating point number.
    type Vkm_Double is new Interfaces.C.double;

    --< The maximum dmmension for a vector or a row or column of a matrix.
    type Vkm_Length is new Integer range 1 .. 4;

    --< The set of indices allowed for use with any vector or matrix.
    type Vkm_Indices is new Integer range 0 .. 3;


    --< @private
    --< Instantiation of Generic Elementary Functions for Float.
    package VKM_FLT_NEF is new
        Ada.Numerics.Generic_Elementary_Functions(Float_Type => Vkm_Float);

    --< @private
    --< Instantiation of Generic Elemantry Functions for Double.
    package VKM_DBL_NEF is new
        Ada.Numerics.Generic_Elementary_Functions(Float_Type => Vkm_Double);

    ----------------------------------------------------------------------------
    -- Conversion Functions
    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Indices.
    --<
    --< @description
    --< Convert a value of type Vkm_Length to a value of type Vkm_Indices.
    --<
    --< @param length The length value to convert to indices.
    --<
    --< @return The length converted to an index.
    ----------------------------------------------------------------------------
    function To_Vkm_Indices (length : in Vkm_Length) return Vkm_Indices is
        (Vkm_Indices(Vkm_Length'Base(length) - 1)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Length.
    --<
    --< @description
    --< Convert a value of type Vkm_Indices to a value of type Vkm_Length.
    --<
    --< @param last_index
    --< The index value to convert to a vector length.
    --<
    --< @return
    --< The result of the conversion.
    ----------------------------------------------------------------------------
    function To_Vkm_Length (last_index : in Vkm_Indices) return Vkm_Length is
        (Vkm_Length(Vkm_Indices'Base(last_index) + 1)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Bool.
    --<
    --< @description
    --< Convert a vulkan math type to the Vkm_Bool type.
    --
    --< If the value is not equal to zero, returns true; Otherwise returns false.
    --
    --< @param     value The value to convert to Vkm_Bool.
    --
    --< @return The conversion to Vkm_Bool.
    ----------------------------------------------------------------------------
    function To_Vkm_Bool (value : in     Vkm_Uint  ) return Vkm_Bool is
        (Vkm_Bool(value /= 0)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Bool.
    --<
    --< @description
    --< Convert a vulkan math type to the Vkm_Bool type.
    --
    --< If the value is not equal to zero, returns true; Otherwise returns false.
    --
    --< @param     value The value to convert to Vkm_Bool.
    --
    --< @return The conversion to Vkm_Bool.
    ----------------------------------------------------------------------------
    function To_Vkm_Bool (value : in     Vkm_Int   ) return Vkm_Bool is
        (Vkm_Bool(value /= 0)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Bool.
    --<
    --< @description
    --< Convert a vulkan math type to the Vkm_Bool type.
    --
    --< If the value is not equal to zero, returns true; Otherwise returns false.
    --
    --< @param     value The value to convert to Vkm_Bool.
    --
    --< @return The conversion to Vkm_Bool.
    ----------------------------------------------------------------------------
    function To_Vkm_Bool (value : in     Vkm_Float ) return Vkm_Bool is
        (Vkm_Bool(value /= 0.0)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Bool.
    --<
    --< @description
    --< Convert a vulkan math type to the Vkm_Bool type.
    --
    --< If the value is not equal to zero, returns true; Otherwise returns false.
    --
    --< @param     value The value to convert to Vkm_Bool.
    --
    --< @return The conversion to Vkm_Bool.
    ----------------------------------------------------------------------------
    function To_Vkm_Bool (value : in     Vkm_Double) return Vkm_Bool is
        (Vkm_Bool(value /= 0.0)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Uint.
    --<
    --< @description
    --< Convert a vulkan math type to the Vkm_Uint type.
    --
    --< If value is true returns 1; Otherwise returns 0.
    --
    --< @param     value The value to convert
    --
    --< @return The conversion to Vkm_Uint.
    ----------------------------------------------------------------------------
    function To_Vkm_Uint (value : in     Vkm_Bool  ) return Vkm_Uint is
        (if value then 1 else 0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Uint.
    --<
    --< @description
    --< Convert a vulkan math type to the Vkm_Uint type.
    --
    --< Conversion from Vkm_Int preserves the bit pattern of the argument, modifying
    --< the value of negative arguments.
    --
    --< @return The conversion to Vkm_Uint.
    ----------------------------------------------------------------------------
    function To_Vkm_Uint is new Ada.Unchecked_Conversion(Source => Vkm_Int, Target => Vkm_Uint);


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Uint.
    --<
    --< @description
    --< Convert a vulkan math type to the Vkm_Uint type.
    --
    --< @param     value The value to convert to Vkm_Uint.
    --
    --< @return The conversion to Vkm_Uint.
    ----------------------------------------------------------------------------
    function To_Vkm_Uint (value : in     Vkm_Float ) return Vkm_Uint is
        (Vkm_Uint(Vkm_Float'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Uint.
    --<
    --< @description
    --< Convert a vulkan math type to the Vkm_Uint type.
    --
    --< @param     value The value to convert to Vkm_Uint.
    --
    --< @return The conversion to Vkm_Uint.
    ----------------------------------------------------------------------------
    function To_Vkm_Uint (value : in     Vkm_Double) return Vkm_Uint is
        (Vkm_Uint(Vkm_Double'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Int.
    --<
    --< @description
    --< Convert various VKM Math types to the Vkm_Int type.
    --
    --< @param     value The value to convert to Vkm_Int.
    --
    --< @return The conversion to Vkm_Int.
    ----------------------------------------------------------------------------
    function To_Vkm_Int (value : in     Vkm_Bool  ) return Vkm_Int is
        (if value then 1 else 0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Int.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Int
    --< math types.
    --
    --< Conversion from Vkm_Uint preserves the bit pattern of the argument,
    --< causing the values of very large unsigned integer to change due to the
    --< sign bit being set.
    --
    --< @return The conversion to Vkm_Int.
    ----------------------------------------------------------------------------
    function To_Vkm_Int is new Ada.Unchecked_Conversion(Source => Vkm_Uint, Target => Vkm_Int);


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Int.
    --<
    --< @description
    --< Convert various VKM Math types to the Vkm_Int type.
    --
    --< @param     value The value to convert to Vkm_Int.
    --
    --< @return The conversion to Vkm_Int.
    ----------------------------------------------------------------------------
    function To_Vkm_Int (value : in     Vkm_Float ) return Vkm_Int is
        (Vkm_Int(Vkm_Float'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Int.
    --<
    --< @description
    --< Convert various VKM Math types to the Vkm_Int type.
    --
    --< @param     value The value to convert to Vkm_Int.
    --
    --< @return The conversion to Vkm_Int.
    ----------------------------------------------------------------------------
    function To_Vkm_Int (value : in     Vkm_Double) return Vkm_Int is
        (Vkm_Int(Vkm_Double'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Float.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Float
    --< math types.
    --
    --< @param     value The value to convert to Vkm_Float.
    --
    --< @return The conversion to Vkm_Float.
    ----------------------------------------------------------------------------
    function To_Vkm_Float (value : in     Vkm_Bool  ) return Vkm_Float is
        (if value then 1.0 else 0.0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Float.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Float
    --< math types.
    --
    --< @param     value The value to convert to Vkm_Float.
    --
    --< @return The conversion to Vkm_Float.
    ----------------------------------------------------------------------------
    function To_Vkm_Float (value : in     Vkm_Uint  ) return Vkm_Float is
        (Vkm_Float(Vkm_Uint'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Float.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Float
    --< math types.
    --
    --< @param     value The value to convert to Vkm_Float.
    --
    --< @return The conversion to Vkm_Float.
    ----------------------------------------------------------------------------
    function To_Vkm_Float (value : in     Vkm_Int   ) return Vkm_Float is
        (Vkm_Float(Vkm_Int'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Float.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Float
    --< math types.
    --
    --< @param     value The value to convert to Vkm_Float.
    --
    --< @return The conversion to Vkm_Float.
    ----------------------------------------------------------------------------
    function To_Vkm_Float (value : in     Vkm_Double) return Vkm_Float is
        (Vkm_Float(Vkm_Double'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Float.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Double
    --< math types.
    --
    --< @param     value The value to convert to Vkm_Double.
    --
    --< @return The conversion to Vkm_Double.
    ----------------------------------------------------------------------------
    function To_Vkm_Double (value : in     Vkm_Bool ) return Vkm_Double is
        (if value then 1.0 else 0.0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Double.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Double
    --< math types.
    --
    --< @param     value The value to convert to Vkm_Double.
    --
    --< @return The conversion to Vkm_Double.
    ----------------------------------------------------------------------------
    function To_Vkm_Double (value : in     Vkm_Uint ) return Vkm_Double is
        (Vkm_Double(Vkm_Uint'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Double.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Double
    --< math types.
    --
    --< @param     value The value to convert to Vkm_Double.
    --
    --< @return The conversion to Vkm_Double.
    ----------------------------------------------------------------------------
    function To_Vkm_Double (value : in     Vkm_Int  ) return Vkm_Double is
        (Vkm_Double(Vkm_Int'Base(value))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert to Vkm_Double.
    --<
    --< @description
    --< The following operations convert various VKM Math types to Vkm_Double
    --< math types.
    --
    --< @param     value The value to convert to Vkm_Double.
    --
    --< @return The conversion to Vkm_Double.
    ----------------------------------------------------------------------------
    function To_Vkm_Double (value : in     Vkm_Float) return Vkm_Double is
        (Vkm_Double(Vkm_Float'Base(value))) with Inline;

    ----------------------------------------------------------------------------
    -- Operator override definitions
    ----------------------------------------------------------------------------
    function "-" (instance : in     Vkm_Bool) return Vkm_Bool is
        (not instance) with inline;
    function "+" (left, right : in     Vkm_Bool) return Vkm_Bool is
        (left xor right) with inline;
    function "-" (left, right : in     Vkm_Bool) return Vkm_Bool is
        (left xor right) with inline;
    function "*" (left, right : in     Vkm_Bool) return Vkm_Bool is
        (left and right) with inline;


    ----------------------------------------------------------------------------

    function "abs" (x : in     Vkm_Float ) return Vkm_Float is
        (if x >= 0.0 then x else -x) with Inline;
    function Floor (x : in     Vkm_Float)  return Vkm_Float renames Vkm_Float'Floor;
    function "mod" (x, y : in     Vkm_Float) return Vkm_Float is
        (x - y * Floor(x / y)) with Inline;
    function Exp (x : in     Vkm_Float) return Vkm_Float
        renames VKM_FLT_NEF.Exp;
    function "**" (x, y : in     Vkm_Float) return Vkm_Float
        renames VKM_FLT_NEF."**";

    ----------------------------------------------------------------------------

    function "abs" (x : in     Vkm_Double ) return Vkm_Double is
        (if x >= 0.0 then x else -x) with Inline;
    function Floor (x : in     Vkm_Double)  return Vkm_Double renames Vkm_Double'Floor;
    function "mod" (x, y : in     Vkm_Double) return Vkm_Double is
        (x - y * Floor(x / y)) with Inline;
    function Exp (x : in     Vkm_Double) return Vkm_Double
        renames VKM_DBL_NEF.Exp;
    function "**" (x, y : in     Vkm_Double) return Vkm_Double
        renames VKM_DBL_NEF."**";

end Vulkan.Math;
