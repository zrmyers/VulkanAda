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
-- This package provides a mapping of the GLSL common functions to Ada's
-- Numerical operations.
--------------------------------------------------------------------------------
package body Vulkan.Math.Common is

    ----------------------------------------------------------------------------
    -- Declarations For Private Operations
    ----------------------------------------------------------------------------
    -- @brief
    -- Algorithm for rounding to the nearest floating point number that is
    -- suitable for an floating point types.
    --
    -- To instantiate, provide a floating point type for which the algorithm
    -- should be made available.
    ----------------------------------------------------------------------------
    generic
        type Floating_Point is digits <>;

    function Compute_RoundEven (x : in     Floating_Point) return Floating_Point;


    ----------------------------------------------------------------------------
    -- @brief
    -- Algorithm for computing the "mod" for a floating point type.
    --
    -- The mod is computed as: ('x' - 'y' * floor ('x' / 'y')).
    --
    -- @param[in]     x, y Input parameters
    --
    -- @return x mod y
    ----------------------------------------------------------------------------
    generic
        type Floating_Point is digits <>;

    function Compute_Mod (x, y : in     Floating_Point) return Floating_Point;


    ----------------------------------------------------------------------------
    -- @brief
    -- Algorithm for computing the "modf" for a floating point type.
    --
    -- This separtes the floating point into its integer part and its fraction
    -- part.
    --
    -- @param[in]     x Mixed integer and fraction number.
    -- @param[out]    i The integer part of 'x'.
    --
    -- @return The fraction part of 'x'
    ----------------------------------------------------------------------------
    generic
        type Floating_Point is digits <>;

    function Compute_Modf (x : in     Floating_Point;
                           i :    out Floating_Point) return Floating_Point;


    ----------------------------------------------------------------------------
    -- @brief
    -- Algorithm for computing the "clamp" for a numeric type.
    --
    -- This ensures that a value is within range [minVal,maxVal]
    --
    -- @param[in]     x      The value to clamp.
    -- @param[in]     minVal Minimum clamp value
    -- @param[in]     maxVal Maximum clamp value
    --
    -- @return The clamped 'x'.
    --
    -- @error
    -- Results are undefined if minVal > maxVal.
    ----------------------------------------------------------------------------
    generic
        type Numeric_Type is private;
        with function Min (x, y : in     Numeric_Type) return Numeric_Type;
        with function Max (x, y : in     Numeric_Type) return Numeric_Type;

    function Compute_Clamp (x      : in     Numeric_Type;
                            minVal : in     Numeric_Type;
                            maxVal : in     Numeric_Type) return Numeric_Type;


    ----------------------------------------------------------------------------
    -- Definitions for Public Operations
    ----------------------------------------------------------------------------


    function "abs" (x : in     Vkm_GenFType) return Vkm_GenFType is

        function Compute_Abs (x : in     Vkm_Float) return Vkm_Float is
            (if x >= 0.0 then x else -x) with Inline;

        function Apply_Abs is new GFT.Apply_Func_IV_RV(Compute_Abs);
    begin
        return Apply_Abs(x);
    end "abs";

    function "abs" (x : in     Vkm_GenDType) return Vkm_GenDType is

        function Compute_Abs (x : in     Vkm_Double) return Vkm_Double is
            (if x >= 0.0 then x else -x) with Inline;

        function Apply_Abs is new GDT.Apply_Func_IV_RV(Compute_Abs);
    begin
        return Apply_Abs(x);
    end "abs";

    function "abs" (x : in     Vkm_GenIType) return Vkm_GenIType is

        function Compute_Abs (x : in     Vkm_Int) return Vkm_Int is
            (if x >= 0 then x else -x) with Inline;

        function Apply_Abs is new GIT.Apply_Func_IV_RV(Compute_Abs);
    begin
        return Apply_Abs(x);
    end "abs";


    ----------------------------------------------------------------------------


    function Sign (x : in     Vkm_GenFType) return Vkm_GenFType is

        function Compute_Sign (x : in Vkm_Float) return Vkm_Float is
            (if x > 0.0 then 1.0 else (if x < 0.0 then -1.0 else 0.0)) with Inline;

        function Apply_Sign is new GFT.Apply_Func_IV_RV(Compute_Sign);

    begin
        return Apply_Sign(x);
    end Sign;


    function Sign (x : in     Vkm_GenDType) return Vkm_GenDType is

        function Compute_Sign (x : in     Vkm_Double) return Vkm_Double is
            (if x > 0.0 then 1.0 else (if x < 0.0 then -1.0 else 0.0)) with Inline;

        function Apply_Sign is new GDT.Apply_Func_IV_RV(Compute_Sign);
    begin
        return Apply_Sign(x);
    end Sign;


    function Sign (x : in     Vkm_GenIType) return Vkm_GenIType is

        function Compute_Sign (x : in     Vkm_Int) return Vkm_Int is
            (if x > 0 then 1 else (if x < 0 then -1 else 0)) with Inline;

        function Apply_Sign is new GIT.Apply_Func_IV_RV(Compute_Sign);
    begin
        return Apply_Sign(x);
    end Sign;


    ----------------------------------------------------------------------------


    function Floor (x : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Floor is new GFT.Apply_Func_IV_RV(Vkm_Float'Floor);
    begin
        return Apply_Floor(x);
    end Floor;


    function Floor (x : in     Vkm_GenDType) return Vkm_GenDType is
        function Apply_Floor is new GDT.Apply_Func_IV_RV(Vkm_Double'Floor);
    begin
        return Apply_Floor(x);
    end Floor;


    ----------------------------------------------------------------------------


    function Trunc (x : in     Vkm_GenFType ) return Vkm_GenFType is
        function Apply_Trunc is new GFT.Apply_Func_IV_RV(Vkm_Float'Truncation);
    begin
        return Apply_Trunc(x);
    end Trunc;


    function Trunc (x : in     Vkm_GenDType) return Vkm_GenDType is
        function Apply_Trunc is new GDT.Apply_Func_IV_RV(Vkm_Double'Truncation);
    begin
        return Apply_Trunc(x);
    end Trunc;


    ----------------------------------------------------------------------------


    function Round (x : in     Vkm_GenFType ) return Vkm_GenFType is
        function Apply_Round is new GFT.Apply_Func_IV_RV(Vkm_Float'Rounding);
    begin
        return Apply_Round(x);
    end Round;


    function Round (x : in     Vkm_GenDType) return Vkm_GenDType is
        function Apply_Round is new GDT.Apply_Func_IV_RV(Vkm_Double'Rounding);
    begin
        return Apply_Round(x);
    end Round;


    ----------------------------------------------------------------------------


    function RoundEven (x : in     Vkm_GenFType) return Vkm_GenFType is

        function Compute_RoundEven_F is new Compute_RoundEven(Vkm_Float);

        function Apply_RoundEven is new GFT.Apply_Func_IV_RV(Compute_RoundEven_F);

    begin
        return Apply_RoundEven(x);
    end RoundEven;

    function RoundEven (x : in     Vkm_GenDType) return Vkm_GenDType is

        function Compute_RoundEven_D is new Compute_RoundEven(Vkm_Double);

        function Apply_RoundEven is new GDT.Apply_Func_IV_RV(Compute_RoundEven_D);
    begin
        return Apply_RoundEven(x);
    end RoundEven;


    ----------------------------------------------------------------------------


    function Ceil (x : in     Vkm_GenFType ) return Vkm_GenFType is
        function Apply_Ceil is new GFT.Apply_Func_IV_RV(Vkm_Float'Ceiling);
    begin
        return Apply_Ceil(x);
    end Ceil;


    function Ceil (x : in     Vkm_GenDType) return Vkm_GenDType is
        function Apply_Ceil is new GDT.Apply_Func_IV_RV(Vkm_Double'Ceiling);
    begin
        return Apply_Ceil(x);
    end Ceil;


    ----------------------------------------------------------------------------


    function Fract (x : in     Vkm_GenFType ) return Vkm_GenFType is

        function Compute_Fract (x : in     Vkm_Float) return Vkm_Float is
            (x - Vkm_Float'Floor(x)) with Inline;

        function Apply_Fract is new GFT.Apply_Func_IV_RV(Compute_Fract);
    begin
        return Apply_Fract(x);
    end Fract;

    function Fract (x : in     Vkm_GenDType) return Vkm_GenDType is

        function Compute_Fract (x : in     Vkm_Double) return Vkm_Double is
            (x - Vkm_Double'Floor(x)) with Inline;

        function Apply_Fract is new GDT.Apply_Func_IV_RV(Compute_Fract);
    begin
        return Apply_Fract(x);
    end Fract;


    ----------------------------------------------------------------------------


    function "mod" (Left, Right : in     Vkm_GenFType ) return Vkm_GenFType is

        function Compute_Mod_F is new Compute_Mod(Vkm_Float);

        function Apply_Mod is new GFT.Apply_Func_IV_IV_RV(Compute_Mod_F);
    begin
        return Apply_Mod(Left, Right);
    end "mod";

    function "mod" (Left, Right : in     Vkm_GenDType) return Vkm_GenDType is

        function Compute_Mod_D is new Compute_Mod(Vkm_Double);

        function Apply_Mod is new GDT.Apply_Func_IV_IV_RV(Compute_Mod_D);

    begin
        return Apply_Mod(Left, Right);
    end "mod";


    ----------------------------------------------------------------------------


    function Modf (x : in     Vkm_GenFType ;
                   i :    out Vkm_GenFType ) return Vkm_GenFType is

        function Compute_Modf_F is new Compute_Modf(Vkm_Float);

        function Apply_Modf is new GFT.Apply_Func_IV_OV_RV(Compute_Modf_F);
    begin

        return Apply_Modf(x,i);
    end Modf;

    function Modf (x : in     Vkm_GenDType;
                   i :    out Vkm_GenDType) return Vkm_GenDType is

        function Compute_Modf_D is new Compute_Modf(Vkm_Double);

        function Apply_Modf is new GDT.Apply_Func_IV_OV_RV(Compute_Modf_D);

    begin
        return Apply_Modf(x,i);
    end Modf;


    ----------------------------------------------------------------------------


    function Min (x, y : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Min is new GFT.Apply_Func_IV_IV_RV(Vkm_Float'Max);
    begin
        return Apply_Min(x,y);
    end Min;


    function Min (x, y : in     Vkm_GenDType) return Vkm_GenDType is
        function Apply_Min is new GDT.Apply_Func_IV_IV_RV(Vkm_Double'Max);
    begin
        return Apply_Min(x,y);
    end Min;


    function Min (x, y : in     Vkm_GenUType) return Vkm_GenUType is
        function Apply_Min is new GUT.Apply_Func_IV_IV_RV(Vkm_Uint'Max);
    begin
        return Apply_Min(x,y);
    end Min;


    function Min (x, y : in     Vkm_GenIType) return Vkm_GenIType is
        function Apply_Min is new GIT.Apply_Func_IV_IV_RV(Vkm_Int'Max);
    begin
        return Apply_Min(x,y);
    end Min;


    ----------------------------------------------------------------------------


    function Max (x, y : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Max is new GFT.Apply_Func_IV_IV_RV(Vkm_Float'Max);
    begin
        return Apply_Max(x,y);
    end Max;


    function Max (x, y : in     Vkm_GenDType) return Vkm_GenDType is
        function Apply_Max is new GDT.Apply_Func_IV_IV_RV(Vkm_Double'Max);
    begin
        return Apply_Max(x,y);
    end Max;


    function Max (x, y : in     Vkm_GenUType) return Vkm_GenUType is
        function Apply_Max is new GUT.Apply_Func_IV_IV_RV(Vkm_Uint'Max);
    begin
        return Apply_Max(x,y);
    end Max;


    function Max (x, y : in     Vkm_GenIType) return Vkm_GenIType is
        function Apply_Max is new GIT.Apply_Func_IV_IV_RV(Vkm_Int'Max);
    begin
        return Apply_Max(x,y);
    end Max;




    ----------------------------------------------------------------------------


    function Clamp (x, minVal, maxVal : in     Vkm_GenFType ) return Vkm_GenFType is

        function Compute_Clamp_F is new Compute_Clamp(Vkm_Float,Vkm_Float'Min,Vkm_Float'Max);

        function Apply_Clamp is new GFT.Apply_Func_IV_IV_IV_RV(Compute_Clamp_F);

    begin
        return Apply_Clamp(x, minVal, maxVal);
    end Clamp;


    function Clamp (x, minVal, maxVal : in     Vkm_GenDType) return Vkm_GenDType is

        function Compute_Clamp_D is new Compute_Clamp(Vkm_Double,Vkm_Double'Min,Vkm_Double'Max);

        function Apply_Clamp is new GDT.Apply_Func_IV_IV_IV_RV(Compute_Clamp_D);

    begin
        return Apply_Clamp(x, minVal, maxVal);
    end Clamp;

    function Clamp (x, minVal, maxVal : in     Vkm_GenUType  ) return Vkm_GenUType is

        function Compute_Clamp_U is new Compute_Clamp(Vkm_Uint,Vkm_Uint'Min,Vkm_Uint'Max);

        function Apply_Clamp is new GUT.Apply_Func_IV_IV_IV_RV(Compute_Clamp_U);

    begin
        return Apply_Clamp(x, minVal, maxVal);
    end Clamp;

    function Clamp (x, minVal, maxVal : in     Vkm_GenIType   ) return Vkm_GenIType is

        function Compute_Clamp_I is new Compute_Clamp(Vkm_Int,Vkm_Int'Min,Vkm_Int'Max);

        function Apply_Clamp is new GIT.Apply_Func_IV_IV_IV_RV(Compute_Clamp_I);

    begin
        return Apply_Clamp(x, minVal, maxVal);
    end Clamp;


    ----------------------------------------------------------------------------
    -- Definitions for Private Operations
    ----------------------------------------------------------------------------


    function Compute_RoundEven (x : in     Floating_Point) return Floating_Point is

        function "mod" is new Compute_Mod(Floating_Point => Floating_Point);

        Result : Floating_Point := Floating_Point'Floor(x);
    begin

        -- If the fractional part of the float is greater than 0.5, round up.
        if x - Result > 0.5 then
            Result := Floating_Point'Ceiling(x);

        -- If the fractional part of the float is exactly 0.5, and the integer
        -- that is below the float is not even, round up.
        elsif x - Result = 0.5 and Result mod 2.0 /= 0.0 then
            Result := Floating_Point'Ceiling(x);

        -- Otherwise the Result has already been obtained.
        end if;
        return Result;
    end Compute_RoundEven;


    ----------------------------------------------------------------------------


    function Compute_Mod (x, y : in     Floating_Point) return Floating_Point is
    begin
        return ((x - y) * Floating_Point'Floor(x / y));
    end Compute_Mod;


    ----------------------------------------------------------------------------


    function Compute_Modf (x : in     Floating_Point;
                           i :    out Floating_Point) return Floating_Point is
    begin
        i := Floating_Point'Floor(x);
        return x - i;
    end Compute_Modf;


    ----------------------------------------------------------------------------


    function Compute_Clamp (x      : in     Numeric_Type;
                            minVal : in     Numeric_Type;
                            maxVal : in     Numeric_Type) return Numeric_Type is
    begin
        return Max(Min(x,maxVal),minVal);
    end Compute_Clamp;

end Vulkan.Math.Common;
