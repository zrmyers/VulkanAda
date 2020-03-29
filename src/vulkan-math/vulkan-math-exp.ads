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

package Vulkan.Math.Exp is
    pragma Preelaborate;
    pragma Pure;


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
    function "**" (x, y : in     Vkm_GenFType) return Vkm_GenFType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes the natural exponentiation of x, e^x.
    --
    -- @param[in]     x The value 'x'
    --
    -- @return Returns e^x.
    ----------------------------------------------------------------------------
    function Exp (x : in     Vkm_GenFType) return Vkm_GenFType;


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
    function Log (x : in     Vkm_GenFType) return Vkm_GenFType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Computes 2 raised to the x power, 2^x.
    --
    -- @param[in]     x The value 'x'
    --
    -- @return Returns 2^x.
    ----------------------------------------------------------------------------
    function Exp2 (x : in     Vkm_GenFType) return Vkm_GenFType;


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
    function Log2 (x : in     Vkm_GenFType) return Vkm_GenFType;


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
    function Sqrt(x : in     Vkm_GenFType ) return Vkm_GenFType;
    function Sqrt(x : in     Vkm_GenDType) return Vkm_GenDType;


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
    function Inverse_Sqrt(x : in     Vkm_GenFType ) return Vkm_GenFType;
    function Inverse_Sqrt(x : in     Vkm_GenDType) return Vkm_GenDType;

end Vulkan.Math.Exp;
