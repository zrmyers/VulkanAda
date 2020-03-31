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
with Vulkan.Math.GenFType;
use Vulkan.Math.GenFType;
with Vulkan.Math.GenDType;
use Vulkan.Math.GenDType;
with Vulkan.Math.GenIType;
use Vulkan.Math.GenIType;
with Vulkan.Math.GenUType;
use Vulkan.Math.GenUType;
with Vulkan.Math.GenBType;
use Vulkan.Math.GenBType;

package Vulkan.Math.Common is
    pragma Preelaborate;
    pragma Pure;

    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the absolute value of x.
    --
    -- @param[in]     x The input parameter
    --
    -- @return Returns x if x >= 0; otherwise it returns -x.
    ----------------------------------------------------------------------------
    function "abs" (x : in     Vkm_GenFType) return Vkm_GenFType;
    function "abs" (x : in     Vkm_GenDType) return Vkm_GenDType;
    function "abs" (x : in     Vkm_GenIType) return Vkm_GenIType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Determines the sign of x.
    --
    -- @param[in]     x The input parameter
    --
    -- @return Returns:
    --    1 if X > 0
    --    0 if X = 0
    --   -1 if x < 0
    ----------------------------------------------------------------------------
    function Sign (x : in     Vkm_GenFType) return Vkm_GenFType;
    function Sign (x : in     Vkm_GenDType) return Vkm_GenDType;
    function Sign (x : in     Vkm_GenIType) return Vkm_GenIType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the floor of x.
    --
    -- @param[in]     x The input parameter
    --
    -- @return Returns the nearest integer y that is less than or equal to x.
    ----------------------------------------------------------------------------
    function Floor (x : in     Vkm_GenFType) return Vkm_GenFType;
    function Floor (x : in     Vkm_GenDType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the truncation of x.
    --
    -- @param[in]     x The input parameter
    --
    -- @return Returns the nearest integer y that whose absolute value is less
    --         than or equal to the absolute value of x.
    ----------------------------------------------------------------------------
    function Trunc (x : in     Vkm_GenFType) return Vkm_GenFType;
    function Trunc (x : in     Vkm_GenDType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Rounds x to the nearest integer. The fraction 0.5 will round away from 0.
    --
    -- @param[in]     x The input parameter.
    --
    -- @return The rounded integer.
    ----------------------------------------------------------------------------
    function Round (x : in     Vkm_GenFType) return Vkm_GenFType;
    function Round (x : in     Vkm_GenDType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Rounds x to the nearest integer. The fraction 0.5 will round toward the
    -- nearest even number.
    --
    -- @param[in]     x The input parameter.
    --
    -- @return The rounded integer.
    ----------------------------------------------------------------------------
    function RoundEven (x : in     Vkm_GenFType) return Vkm_GenFType;
    function RoundEven (x : in     Vkm_GenDType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the ceil of x.
    --
    -- @param[in]     x The input parameter.
    --
    -- @return Returns the nearest integer that is greater than or equal to x.
    ----------------------------------------------------------------------------
    function Ceil (x : in     Vkm_GenFType) return Vkm_GenFType;
    function Ceil (x : in     Vkm_GenDType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Get the fraction part of x.
    --
    -- @param[in]     x The input parameter.
    --
    -- @return Returns x - floor(x)
    ----------------------------------------------------------------------------
    function Fract (x : in     Vkm_GenFType) return Vkm_GenFType;
    function Fract (x : in     Vkm_GenDType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Compute the mod of x and y.
    --
    -- @param[in]    Left  The left argument to the mod operator.
    -- @param[in]    Right The Right argument to the mod operator.
    --
    -- @return Returns Left - Right * floor (Left / Right)
    ----------------------------------------------------------------------------
    function "mod" (Left, Right : in     Vkm_GenFType) return Vkm_GenFType;
    function "mod" (Left, Right : in     Vkm_GenDType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Compute the modf of x and y.
    --
    -- @param[in]    x The input parameter
    -- @param[out]   i The integer part of x.
    --
    -- @return Returns the fraction part of x.
    ----------------------------------------------------------------------------
    function Modf (x : in     Vkm_GenFType;
                   i :    out Vkm_GenFType) return Vkm_GenFType;
    function Modf (x : in     Vkm_GenDType;
                   i :    out Vkm_GenDType) return Vkm_GenDType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Compute the min between the two values x and y.
    --
    -- @param[in]     x The input parameter 'x'.
    -- @param[in]     y The input parameter 'y'.
    --
    -- @return Returns the smaller of the two numbers.
    ----------------------------------------------------------------------------
    function Min (x, y : in     Vkm_GenFType) return Vkm_GenFType;
    function Min (x, y : in     Vkm_GenDType) return Vkm_GenDType;
    function Min (x, y : in     Vkm_GenUType) return Vkm_GenUType;
    function Min (x, y : in     Vkm_GenIType) return Vkm_GenIType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Compute the min between the two values x and y.
    --
    -- @param[in]     x The input parameter 'x'.
    -- @param[in]     y The input parameter 'y'.
    --
    -- @return Returns the smaller of the two numbers.
    ----------------------------------------------------------------------------
    function Max (x, y : in     Vkm_GenFType) return Vkm_GenFType;
    function Max (x, y : in     Vkm_GenDType) return Vkm_GenDType;
    function Max (x, y : in     Vkm_GenUType) return Vkm_GenUType;
    function Max (x, y : in     Vkm_GenIType) return Vkm_GenIType;


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
    function Clamp (x, minVal, maxVal : in     Vkm_GenFType) return Vkm_GenFType;
    function Clamp (x                 : in     Vkm_GenFType;
                    minVal, maxVal    : in     Vkm_Float   ) return Vkm_GenFType;
    function Clamp (x, minVal, maxVal : in     Vkm_GenDType) return Vkm_GenDType;
    function Clamp (x                 : in     Vkm_GenDType;
                    minVal, maxVal    : in     Vkm_Double  ) return Vkm_GenDType;
    function Clamp (x, minVal, maxVal : in     Vkm_GenUType) return Vkm_GenUType;
    function Clamp (x, minVal, maxVal : in     Vkm_GenIType) return Vkm_GenIType;


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
    function Mix (x, y, a : in     Vkm_GenFType) return Vkm_GenFType;
    function Mix (x, y    : in     Vkm_GenFType;
                  a       : in     Vkm_Float   ) return Vkm_GenFType;
    function Mix (x, y, a : in     Vkm_GenDType) return Vkm_GenDType;
    function Mix (x, y    : in     Vkm_GenDType;
                  a       : in     Vkm_Double  ) return Vkm_GenDType;


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
    function Mix (x, y : in     Vkm_GenFType;
                  a    : in     Vkm_GenBType) return Vkm_GenFType;
    function Mix (x, y : in     Vkm_GenDType;
                  a    : in     Vkm_GenBType) return Vkm_GenDType;
    function Mix (x, y : in     Vkm_GenUType;
                  a    : in     Vkm_GenBType) return Vkm_GenUType;
    function Mix (x, y : in     Vkm_GenIType;
                  a    : in     Vkm_GenBType) return Vkm_GenIType;
    function Mix (x, y : in     Vkm_GenBType;
                  a    : in     Vkm_GenBType) return Vkm_GenBType;

end Vulkan.Math.Common;
