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
with Vulkan.Math.GenIType;
with Vulkan.Math.GenUType;
with Vulkan.Math.GenBType;
with Vulkan.Math.Numerics;
with ada.Unchecked_Conversion;

-- Uses
use Vulkan.Math.GenFType;
use Vulkan.Math.GenDType;
use Vulkan.Math.GenIType;
use Vulkan.Math.GenUType;
use Vulkan.Math.GenBType;

--------------------------------------------------------------------------------
--< @group Vulkan Math Functions
--------------------------------------------------------------------------------
--< @summary
--< This package provides GLSL Common Built-in functions.
--<
--< @description
--< All common functions operate component-wise.
--------------------------------------------------------------------------------
package Vulkan.Math.Common is
    pragma Preelaborate;
    pragma Pure;

    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Computes the absolute value of scalar Vkm_Float x.
    --<
    --< @param x 
    --< The input parameter
    --<
    --< @return 
    --< Returns x if x >= 0.0; otherwise it returns -x.
    ----------------------------------------------------------------------------
    function Absolute_Value (x : in     Vkm_Float ) return Vkm_Float is
        (if x >= 0.0 then x else -x) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Computes the absolute value of scalar Vkm_Double x.
    --<
    --< @param x 
    --< The input parameter
    --<
    --< @return 
    --< Returns x if x >= 0.0; otherwise it returns -x.
    ----------------------------------------------------------------------------
    function Absolute_Value (x : in     Vkm_Double) return Vkm_Double is
        (if x >= 0.0 then x else -x) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Computes the absolute value of scalar Vkm_Int x.
    --<
    --< @param x 
    --< The input parameter
    --<
    --< @return 
    --< Returns x if x >= 0; otherwise it returns -x.
    ----------------------------------------------------------------------------
    function Absolute_Value (x : in     Vkm_Int) return Vkm_Int is
        (if x >= 0 then x else -x)   with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Applies Absolute_Value() component-wise on a GenFType vector, returning
    --< a GenFType vector with the result.
    ----------------------------------------------------------------------------
    function Absolute_Value is new GFT.Apply_Func_IV_RV(Absolute_Value);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Applies Absolute_Value() component-wise on a GenDType vector, returning
    --< a GenDType vector with the result.
    ----------------------------------------------------------------------------
    function Absolute_Value is new GDT.Apply_Func_IV_RV(Absolute_Value);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Applies Absolute_Value() component-wise on a GenIType vector, returning
    --< a GenIType vector with the result.
    ----------------------------------------------------------------------------
    function Absolute_Value is new GIT.Apply_Func_IV_RV(Absolute_Value);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of a scalar Vkm_Float.
    --<
    --< @param x 
    --< The scalar input parameter
    --<
    --< @return 
    --< Returns one of the following:
    --< -  1 if X > 0
    --< -  0 if X = 0
    --< - -1 if x < 0
    ----------------------------------------------------------------------------
    function Sign (x : in     Vkm_Float ) return Vkm_Float is
        (if x > 0.0 then 1.0 elsif x < 0.0 then -1.0 else 0.0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of a scalar Vkm_Double.
    --<
    --< @param x 
    --< The scalar input parameter
    --<
    --< @return 
    --< Returns one of the following:
    --< -  1 if X > 0
    --< -  0 if X = 0
    --< - -1 if x < 0
    ----------------------------------------------------------------------------
    function Sign (x : in     Vkm_Double) return Vkm_Double is
        (if x > 0.0 then 1.0 elsif x < 0.0 then -1.0 else 0.0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of a scalar Vkm_Int.
    --<
    --< @param x 
    --< The scalar input parameter
    --<
    --< @return 
    --< Returns one of the following:
    --< -  1 if X > 0
    --< -  0 if X = 0
    --< - -1 if x < 0
    ----------------------------------------------------------------------------
    function Sign (x : in     Vkm_Int   ) return Vkm_Int is
        (if x > 0 then 1 elsif x < 0 then -1 else 0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of each component of GenFType vector x.
    ----------------------------------------------------------------------------
    function Sign is new GFT.Apply_Func_IV_RV(Sign);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of each component of GenDType vector x.
    ----------------------------------------------------------------------------
    function Sign is new GDT.Apply_Func_IV_RV(Sign);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of each component of GenDType vector x.
    ----------------------------------------------------------------------------
    function Sign is new GIT.Apply_Func_IV_RV(Sign);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the floor of x.
    --<
    --< @description
    --< Computes the floor, y, as the nearest integer that is less than or equal
    --< to scalar Vkm_Float x.
    --<
    --< @param x 
    --< The value for which the floor is computed.
    --<
    --< @return 
    --< Returns the floor, y.
    ----------------------------------------------------------------------------
    function Floor (x : in     Vkm_Float)  return Vkm_Float renames Vkm_Float'Floor;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the floor of x.
    --<
    --< @description
    --< Computes the floor, y, as the nearest integer that is less than or equal
    --< to scalar Vkm_Double x.
    --<
    --< @param x 
    --< The value for which the floor is computed.
    --<
    --< @return 
    --< Returns the floor, y.
    ----------------------------------------------------------------------------
    function Floor (x : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Floor;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the floor of x.
    --<
    --< @description
    --< Computes the floor for each component of the GenFType, returning a vector
    --< containing the component-wise result.
    ----------------------------------------------------------------------------
    function Floor is new GFT.Apply_Func_IV_RV(Floor);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the floor of x.
    --<
    --< @description
    --< Computes the floor for each component of the GenDType, returning a vector
    --< containing the component-wise result.
    ----------------------------------------------------------------------------
    function Floor is new GDT.Apply_Func_IV_RV(Floor);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the truncation of x.
    --<
    --< @description
    --< Computes the trunction of x, y, as the nearest integer to x whose absolute
    --< value is less than or equal to the absolute value of x.
    --<
    --< @param x
    --< The value on which truncation is performed.
    --<
    --< @return 
    --< The truncation of x, y.
    ----------------------------------------------------------------------------
    function Trunc (x : in     Vkm_Float) return Vkm_Float renames Vkm_Float'Truncation;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the truncation of x.
    --<
    --< @description
    --< Computes the trunction of x, y, as the nearest integer to x whose absolute
    --< value is less than or equal to the absolute value of x.
    --<
    --< @param x
    --< The value on which truncation is performed.
    --<
    --< @return 
    --< The truncation of x, y.
    ----------------------------------------------------------------------------
    function Trunc (x : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Truncation;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the truncation of x.
    --<
    --< @description
    --< Computes component-wise trunction on a vector x, returning a vector
    --< with the result.
    ----------------------------------------------------------------------------
    function Trunc is new GFT.Apply_Func_IV_RV(Trunc);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the truncation of x.
    --<
    --< @description
    --< Computes component-wise trunction on a vector x, returning a vector
    --< with the result.
    ----------------------------------------------------------------------------
    function Trunc is new GDT.Apply_Func_IV_RV(Trunc);


    ----------------------------------------------------------------------------
    -- @brief
    -- Rounds x to the nearest integer. The fraction 0.5 will round away from 0.
    --
    -- @param[in]     x The input parameter.
    --
    -- @return The rounded integer.
    ----------------------------------------------------------------------------
    function Round (x : in     Vkm_Float   ) return Vkm_Float renames Vkm_Float'Rounding;
    function Round (x : in     Vkm_Double  ) return Vkm_Double renames Vkm_Double'Rounding;
    function Round is new GFT.Apply_Func_IV_RV(Round);
    function Round is new GDT.Apply_Func_IV_RV(Round);


    ----------------------------------------------------------------------------
    -- @brief
    -- Rounds x to the nearest integer. The fraction 0.5 will round toward the
    -- nearest even number.
    --
    -- @param[in]     x The input parameter.
    --
    -- @return The rounded integer.
    ----------------------------------------------------------------------------
    function RoundEven (x : in     Vkm_Float) return Vkm_Float renames Vkm_Float'Unbiased_Rounding;
    function RoundEven (x : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Unbiased_Rounding;
    function RoundEven is new GFT.Apply_Func_IV_RV(RoundEven);
    function RoundEven is new GdT.Apply_Func_IV_RV(RoundEven);


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the ceil of x.
    --
    -- @param[in]     x The input parameter.
    --
    -- @return Returns the nearest integer that is greater than or equal to x.
    ----------------------------------------------------------------------------
    function Ceil (x : in     Vkm_Float   ) return Vkm_Float renames Vkm_Float'Ceiling;
    function Ceil (x : in     Vkm_Double  ) return Vkm_Double renames Vkm_Double'Ceiling;
    function Ceil is new GFT.Apply_Func_IV_RV(Ceil);
    function Ceil is new GDT.Apply_Func_IV_RV(Ceil);


    ----------------------------------------------------------------------------
    -- @brief
    -- Get the fraction part of x.
    --
    -- @param[in]     x The input parameter.
    --
    -- @return Returns x - floor(x)
    ----------------------------------------------------------------------------
    function Fract (x : in     Vkm_Float   ) return Vkm_Float is
        (x - Floor(x)) with Inline;
    function Fract (x : in     Vkm_Double  ) return Vkm_Double is
        (x - Floor(x)) with Inline;
    function Fract is new GFT.Apply_Func_IV_RV(Fract);
    function Fract is new GDT.Apply_Func_IV_RV(Fract);


    ----------------------------------------------------------------------------
    -- @brief
    -- Compute the mod of x and y.
    --
    -- @param[in]    Left  The left argument to the mod operator.
    -- @param[in]    Right The Right argument to the mod operator.
    --
    -- @return Returns Left - Right * floor (Left / Right)
    ----------------------------------------------------------------------------
    function Modulo (Left, Right : in     Vkm_Float) return Vkm_Float is
        ((Left - Right) * Floor(Left / Right)) with Inline;
    function Modulo (Left, Right : in     Vkm_Double) return Vkm_Double is
        ((Left - Right) * Floor(Left / Right)) with Inline;
    function Modulo is new GFT.Apply_Func_IV_IV_RV(Modulo);
    function Modulo is new GDT.Apply_Func_IV_IV_RV(Modulo);


    ----------------------------------------------------------------------------
    --< @brief
    --< Compute the modf of x and y.
    --<
    --< @param x The input parameter
    --< @param i The integer part of x.
    --<
    --< @return Returns the fraction part of x.
    ----------------------------------------------------------------------------
    function Modf is new Vulkan.Math.Numerics.Compute_Modf(Vkm_Float);
    function Modf is new Vulkan.Math.Numerics.Compute_Modf(Vkm_Double);
    function Modf is new GFT.Apply_Func_IV_OV_RV(Modf);
    function Modf is new GDT.Apply_Func_IV_OV_RV(Modf);


    ----------------------------------------------------------------------------
    -- @brief
    -- Compute the min between the two values x and y.
    --
    -- @param[in]     x The input parameter 'x'.
    -- @param[in]     y The input parameter 'y'.
    --
    -- @return Returns the smaller of the two numbers.
    ----------------------------------------------------------------------------
    function Min (x, y : in     Vkm_Float ) return Vkm_Float  renames Vkm_Float'Min;
    function Min (x, y : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Min;
    function Min (x, y : in     Vkm_Uint  ) return Vkm_Uint   renames Vkm_Uint'Min;
    function Min (x, y : in     Vkm_Int   ) return Vkm_Int    renames Vkm_Int'Min;
    function Min is new GFT.Apply_Func_IV_IV_RV(Min);
    function Min is new GDT.Apply_Func_IV_IV_RV(Min);
    function Min is new GUT.Apply_Func_IV_IV_RV(Min);
    function Min is new GIT.Apply_Func_IV_IV_RV(Min);


    ----------------------------------------------------------------------------
    -- @brief
    -- Compute the min between the two values x and y.
    --
    -- @param[in]     x The input parameter 'x'.
    -- @param[in]     y The input parameter 'y'.
    --
    -- @return Returns the smaller of the two numbers.
    ----------------------------------------------------------------------------
    function Max (x, y : in     Vkm_Float ) return Vkm_Float  renames Vkm_Float'Max;
    function Max (x, y : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Max;
    function Max (x, y : in     Vkm_Uint  ) return Vkm_Uint   renames Vkm_Uint'Max;
    function Max (x, y : in     Vkm_Int   ) return Vkm_Int    renames Vkm_Int'Max;
    function Max is new GFT.Apply_Func_IV_IV_RV(Max);
    function Max is new GDT.Apply_Func_IV_IV_RV(Max);
    function Max is new GUT.Apply_Func_IV_IV_RV(Max);
    function Max is new GIT.Apply_Func_IV_IV_RV(Max);


    ----------------------------------------------------------------------------
    -- @brief
    -- Clamp x between minVal and maxVal.
    --
    -- @param[in]     x      The input parameter 'x'.
    -- @param[in]     minVal The minimum value in range.
    -- @param[in]     maxVal The maximum value in range.
    --
    -- @return Returns:
    --    x,      if x >= minVal and x <= maxVal
    --    minVal, if x <  minVal
    --    maxVal, if x >  maxVal
    --
    -- @errors
    -- Results are undefined for minVal > maxVal.
    ----------------------------------------------------------------------------
    function Clamp (x, minVal, maxVal : in     Vkm_Float)  return Vkm_Float is
        (Min(Max(x,minVal),maxVal)) with Inline;
    function Clamp (x, minVal, maxVal : in     Vkm_Double) return Vkm_Double is
        (Min(Max(x,minVal),maxVal)) with Inline;
    function Clamp (x, minVal, maxVal : in     Vkm_Uint)   return Vkm_Uint is
        (Min(Max(x,minVal),maxVal)) with Inline;
    function Clamp (x, minVal, maxVal : in     Vkm_Int)    return Vkm_Int is
        (Min(Max(x,minVal),maxVal)) with Inline;
    function Clamp is new GFT.Apply_Func_IV_IV_IV_RV(Clamp);
    function Clamp is new GDT.Apply_Func_IV_IV_IV_RV(Clamp);
    function Clamp is new GUT.Apply_Func_IV_IV_IV_RV(Clamp);
    function Clamp is new GIT.Apply_Func_IV_IV_IV_RV(Clamp);
    function Clamp is new GFT.Apply_Func_IV_IS_IS_RV(Clamp);
    function Clamp is new GDT.Apply_Func_IV_IS_IS_RV(Clamp);

    ----------------------------------------------------------------------------
    -- @brief
    -- Mix the values 'x' and 'y' together using a linear blend function.
    --
    -- The linear blend function is 'x * (1 - a) + y * a'
    --
    -- @param[in]     x The input parameter 'x' that is mixed with 'y'
    -- @param[in]     y The input paramter 'y' that is mixed with 'x'
    -- @param[in]     a The input parameter 'a' which is a coefficient in the
    --                  linear blend function.
    --
    -- @return X mixed with y.
    ----------------------------------------------------------------------------
    function Mix (x, y, a : in     Vkm_Float) return Vkm_Float is
        (x * (1.0 - a) + y * a) with Inline;
    function Mix (x, y, a : in     Vkm_Double) return Vkm_Double is
        (x * (1.0 - a) + y * a) with Inline;
    function Mix is new GFT.Apply_Func_IV_IV_IV_RV(Mix);
    function Mix is new GFT.Apply_Func_IV_IV_IS_RV(Mix);
    function Mix is new GDT.Apply_Func_IV_IV_IV_RV(Mix);
    function Mix is new GDT.Apply_Func_IV_IV_IS_RV(Mix);


    ----------------------------------------------------------------------------
    -- @brief
    -- Mix the values 'x' and 'y' together using a boolean blend function.
    --
    -- The boolean blend function is applied component-wise as follows:
    --     x if a is true
    --     y if a is false
    --
    -- @param[in]     x The input parameter 'x' that is mixed with 'y'
    -- @param[in]     y The input parameter 'y' that is mixed with 'x'
    -- @param[in]     a The input parameter 'a' which is the boolean mixing
    --                  coefficient.
    --
    -- @returns The mixture of x with y.
    ----------------------------------------------------------------------------
    function Mix (x, y : in     Vkm_Float;
                  a    : in     Vkm_Bool) return Vkm_Float is
        (if a then x else y ) with Inline;
    function Mix (x, y : in     Vkm_Double;
                  a    : in     Vkm_Bool) return Vkm_Double is
        (if a then x else y ) with Inline;
    function Mix (x, y : in     Vkm_Uint;
                  a    : in     Vkm_Bool) return Vkm_Uint is
        (if a then x else y ) with Inline;
    function Mix (x, y : in     Vkm_Int;
                  a    : in     Vkm_Bool) return Vkm_Int is
        (if a then x else y ) with Inline;
    function Mix (x, y : in     Vkm_Bool;
                  a    : in     Vkm_Bool) return Vkm_Bool is
        (if a then x else y ) with Inline;
    function Mix is new Apply_Func_IVF_IVF_IVB_RVF(Mix);
    function Mix is new Apply_Func_IVD_IVD_IVB_RVD(Mix);
    function Mix is new Apply_Func_IVI_IVI_IVB_RVI(Mix);
    function Mix is new Apply_Func_IVU_IVU_IVB_RVU(Mix);
    function Mix is new GBT.Apply_Func_IV_IV_IV_RV(Mix);


    ----------------------------------------------------------------------------
    -- @brief
    -- Step function.
    --
    -- For each component of edge:
    --     y = 0.0 if x <  edge
    --     y = 1.0 if x >= edge
    --
    -- @param[in]     edge
    -- @param[in]     x
    --
    -- @returns y
    ----------------------------------------------------------------------------
    function Step (edge, x : in     Vkm_Float) return Vkm_Float is
        (if x < edge then 0.0 else 1.0) with Inline;
    function Step (edge, x : in     Vkm_Double) return Vkm_Double is
        (if x < edge then 0.0 else 1.0) with Inline;
    function Step is new GFT.Apply_Func_IV_IV_RV(Step);
    function Step is new GFT.Apply_Func_IV_IS_RV(Step);
    function Step is new GDT.Apply_Func_IV_IV_RV(Step);
    function Step is new GDT.Apply_Func_IV_IS_RV(Step);


    ----------------------------------------------------------------------------
    -- @brief
    -- Smooth step function.
    --
    -- Compute:  t = clamp ((x - edge0) / (edge1 - edge0), 0, 1).
    --           t = t^2(3 - 2t)
    --
    -- @param[in]     edge0 The lower edge.
    -- @param[in]     edge1 The upper edge.
    -- @param[in]     x     The value to compute the smooth step of.
    --
    -- @return t
    ----------------------------------------------------------------------------
    function Smooth_Step is new Vulkan.Math.Numerics.Smooth_Step(Vkm_Float,Clamp);
    function Smooth_Step is new GFT.Apply_Func_IV_IV_IV_RV(Smooth_Step);
    function Smooth_Step is new GFT.Apply_Func_IV_IV_IS_RV(Smooth_Step);
    function Smooth_Step is new Vulkan.Math.Numerics.Smooth_Step(Vkm_Double,Clamp);
    function Smooth_Step is new GDT.Apply_Func_IV_IV_IV_RV(Smooth_Step);
    function Smooth_Step is new GDT.Apply_Func_IV_IV_IS_RV(Smooth_Step);


    ----------------------------------------------------------------------------
    -- @brief
    -- Determine whether the input holds a NaN. Always returns false in Ada.
    --
    -- @returns False, always
    ----------------------------------------------------------------------------
    function Is_Nan is new Vulkan.Math.Numerics.Is_Nan(Vkm_Float);
    function Is_Nan is new Vulkan.Math.Numerics.Is_Nan(Vkm_Double);
    function Is_Nan is new Apply_Func_IVF_RVB(Is_Nan);
    function Is_Nan is new Apply_Func_IVD_RVB(Is_Nan);


    ----------------------------------------------------------------------------
    -- @brief
    -- Determine whether the input holds an Inf. Always returns false in Ada.
    --
    -- @returns False, always.
    ----------------------------------------------------------------------------
    function Is_Inf is new Vulkan.Math.Numerics.Is_Inf(Vkm_Float);
    function Is_Inf is new Vulkan.Math.Numerics.Is_Inf(Vkm_Double);
    function Is_Inf is new Apply_Func_IVF_RVB(Is_Inf);
    function Is_Inf is new Apply_Func_IVD_RVB(Is_Inf);


    ----------------------------------------------------------------------------
    -- @brief
    -- Convert the floating point value to a signed or unsigned integer that
    -- represents the encoding for the floating point value.
    --
    -- @param[in]     value The floating point value.
    --
    -- @returns The signed or unsigned integer representation of the float.
    ----------------------------------------------------------------------------
    function Float_Bits_To_Int is new
        Ada.Unchecked_Conversion(Source => Vkm_Float, Target => Vkm_Int);
    function Float_Bits_To_Uint is new
        Ada.Unchecked_Conversion(Source => Vkm_Float, Target => Vkm_Uint);
    function Float_Bits_To_Int is new Apply_Func_IVF_RVI(Float_Bits_To_Int);
    function Float_Bits_To_Uint is new Apply_Func_IVF_RVU(Float_Bits_To_Uint);


    ----------------------------------------------------------------------------
    -- @brief
    -- Convert the floating point value to a signed or unsigned integer that
    -- represents the encoding for the floating point value.
    --
    -- @param[in]     value The floating point value.
    --
    -- @returns The signed or unsigned integer representation of the float.
    ----------------------------------------------------------------------------
    function Int_Bits_To_Float is new
        Ada.Unchecked_Conversion(Source => Vkm_Int, Target => Vkm_Float);
    function Uint_Bits_To_Float is new
        Ada.Unchecked_Conversion(Source => Vkm_Uint, Target => Vkm_Float);
    function Int_Bits_To_Float is new Apply_Func_IVI_RVF(Int_Bits_To_Float);
    function Uint_Bits_To_Float is new Apply_Func_IVU_RVF(Uint_Bits_To_Float);


    ----------------------------------------------------------------------------
    -- @brief
    -- Compute a fused multiply add operation.
    --
    -- @param[in]     a, b, c The parameters for the computation.
    --
    -- @return a * b + c
    ----------------------------------------------------------------------------
    function Fma(a, b, c : in     Vkm_Float) return Vkm_Float is
        (a * b + c) with Inline;
    function Fma is new GFT.Apply_Func_IV_IV_IV_RV(Fma);
    function Fma(a, b, c : in     Vkm_Double) return Vkm_Double is
        (a * b + c) with Inline;
    function Fma is new GDT.Apply_Func_IV_IV_IV_RV(Fma);


    ----------------------------------------------------------------------------
    -- @brief
    -- Splits the floating point value x into its significand and exponent parts.
    --
    --     x = significand * 2^exponent
    --
    -- @param[in]     x        The value to split.
    -- @param[out]    exponent The exponent of x.
    --
    -- @return The significand of x.
    ----------------------------------------------------------------------------
    function Frexp is new Vulkan.Math.Numerics.Frexp(Vkm_Float);
    function Frexp is new Vulkan.Math.Numerics.Frexp(Vkm_Double);
    function Frexp is new Apply_Func_IVF_OVI_RVF(Frexp);
    function Frexp is new Apply_Func_IVD_OVI_RVD(Frexp);


    ----------------------------------------------------------------------------
    -- @brief
    -- This operation composes a floting point number from a fraction value and
    -- an exponent value.
    --
    --     x = significand * 2^exponent
    --
    -- @param[in]     significand The significand.
    -- @param[in]     exponent    The exponent.
    --
    -- @returns x
    ----------------------------------------------------------------------------
    function Ldexp is new Vulkan.Math.Numerics.Ldexp(Vkm_Float);
    function Ldexp is new Vulkan.Math.Numerics.Ldexp(Vkm_Double);
    function Ldexp is new Apply_Func_IVF_IVI_RVF(Ldexp);
    function Ldexp is new Apply_Func_IVD_IVI_RVD(Ldexp);

end Vulkan.Math.Common;
