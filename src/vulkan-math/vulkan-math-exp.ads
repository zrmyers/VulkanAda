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
-- This package provides a mapping of the GLSL exponential functions to Ada's
-- Numerical operations.
--------------------------------------------------------------------------------
with Vulkan.Math.GenFType;
use Vulkan.Math.GenFType;

with Vulkan.Math.GenDType;
use Vulkan.Math.GenDType;

with Vulkan.Math.Numerics;

package Vulkan.Math.Exp is
    pragma Preelaborate;
    pragma Pure;

    -- Ln(2)
    LN2 : constant := 0.69314_71805_59945_30941_72321_21458_18;

    ----------------------------------------------------------------------------
    -- @brief
    -- Computes x raised to the y power.
    --
    -- @param[in]     x The value that is raised to a power.
    -- @param[in]     y The power that 'x' is raised to.
    --
    -- @return Returns x^y.
    --
    -- @error
    -- Results are undefined for x < 0 or x=0 and y <= 0.
    ----------------------------------------------------------------------------
    function Pow (x, y : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF."**";
    function Pow is new GFT.Apply_Func_IV_IV_RV(Pow);
    function Pow (x, y : in     Vkm_Double) return Vkm_Double
        renames Vulkan.Math.Numerics.VKM_DBL_NEF."**";
    function Pow is new GDT.Apply_Func_IV_IV_RV(Pow);

    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the natural exponentiation of x, e^x.
    --
    -- @param[in]     x The value 'x'
    --
    -- @return Returns e^x.
    ----------------------------------------------------------------------------
    function Exp (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Exp;
    function Exp is new GFT.Apply_Func_IV_RV(Exp);


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the natural logarithm of x, which satisfies equation x=e^y.
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return Returns ln(x).
    --
    -- @error
    -- Results are undefined if x <= 0.
    ----------------------------------------------------------------------------
    function Log (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Log;
    function Log is new GFT.Apply_Func_IV_RV(Log);


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes 2 raised to the x power, 2^x.
    --
    -- @param[in]     x The value 'x'
    --
    -- @return Returns 2^x.
    ----------------------------------------------------------------------------
    function Exp2 (x : in     Vkm_Float) return Vkm_Float is
        (Exp( LN2 * x)) with Inline;
    function Exp2 is new GFT.Apply_Func_IV_RV(Exp2);


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes log base 2 of x, finding the value y which satisfies y = 2^x.
    --
    -- @param[in]     x The value 'x'
    --
    -- @returns y = 2^x.
    --
    -- @error
    -- Results are undefined for x <= 0.
    ----------------------------------------------------------------------------
    function Log2 (x : in     Vkm_Float) return Vkm_Float is
        (Exp(x) / LN2) with Inline;
    function Log2 is new GFT.Apply_Func_IV_RV(Log2);


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the square root of x.
    --
    -- @param[in]     x The value 'x'
    --
    -- @returns y = sqrt(x)
    --
    -- @error
    -- Results are undefined for x < 0.
    ----------------------------------------------------------------------------
    function Sqrt (x : in     Vkm_Float ) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Sqrt;
    function Sqrt is new GFT.Apply_Func_IV_RV(Sqrt);
    function Sqrt (x : in     Vkm_Double) return Vkm_Double
        renames Vulkan.Math.Numerics.VKM_DBL_NEF.Sqrt;
    function Sqrt is new GDT.Apply_Func_IV_RV(Sqrt);


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the inverse square root of x.
    --
    -- @param[in]     x The value 'x'
    --
    -- @returns y = 1/sqrt(x)
    --
    -- @error
    -- Results are undefined for x <= 0.
    ----------------------------------------------------------------------------
    function Inverse_Sqrt(x : in     Vkm_Float ) return Vkm_Float is
        (1.0 / Sqrt(x)) with Inline;
    function Inverse_Sqrt(x : in     Vkm_Double) return Vkm_Double is
        (1.0 / Sqrt(x)) with Inline;
    function Inverse_Sqrt is new GFT.Apply_Func_IV_RV(Inverse_Sqrt);
    function Inverse_Sqrt is new GDT.Apply_Func_IV_RV(Inverse_Sqrt);


end Vulkan.Math.Exp;
