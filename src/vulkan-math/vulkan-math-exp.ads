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
with Vulkan.Math.Numerics;

use Vulkan.Math.GenFType;
use Vulkan.Math.GenDType;

--------------------------------------------------------------------------------
--< @group Vulkan Math Functions
--------------------------------------------------------------------------------
--< @summary
--< This package provides GLSL Exponential Built-in functions.
--<
--< @description
--< All exponential functions operate component-wise on vectors.
--------------------------------------------------------------------------------
package Vulkan.Math.Exp is
    pragma Preelaborate;
    pragma Pure;

    --< The constant natural logarithm of 2 value. This constant is used in the
    --< implementation of Exp2().
    LN2 : constant := 0.69314_71805_59945_30941_72321_21458_18;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute x raised to the y power.
    --<
    --< @description
    --< Compute x raised to the y power for single precision floating point 
    --< numbers.
    --<
    --< @param x 
    --< The value that is raised to a power
    --<
    --< @param y 
    --< The power that 'x' is raised to.
    --<
    --< @return 
    --< The result of (x ** y).
    ----------------------------------------------------------------------------
    function Pow (x, y : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF."**";


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute x raised to the y power.
    --<
    --< @description
    --< Compute x raised to the y power for double precision floating point 
    --< numbers.
    --<
    --< @param x 
    --< The value that is raised to a power
    --<
    --< @param y 
    --< The power that 'x' is raised to.
    --<
    --< @return 
    --< The result of (x ** y).
    ----------------------------------------------------------------------------
    function Pow (x, y : in     Vkm_Double) return Vkm_Double
        renames Vulkan.Math.Numerics.VKM_DBL_NEF."**";


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute x raised to the y power, component-wise.
    --<
    --< @description
    --< Compute x raised to the y power component-wise for two GenFType vectors
    --< of the same length.
    ----------------------------------------------------------------------------
    function Pow is new GFT.Apply_Func_IV_IV_RV(Pow);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute x raised to the y power, component-wise.
    --<
    --< @description
    --< Compute x raised to the y power component-wise for two GenDType vectors
    --< of the same length.
    ----------------------------------------------------------------------------
    function Pow is new GDT.Apply_Func_IV_IV_RV(Pow);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the natural exponentiation of x, e^x.
    --<
    --< @description
    --< Computes the natural exponentiation of x, e^x for a single precision
    --< floating point number.
    --< 
    --< @param x 
    --< The value 'x'
    --< 
    --< @return 
    --< The result of e^x.
    ----------------------------------------------------------------------------
    function Exp (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Exp;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the natural exponentiation of x, component-wise.
    --<
    --< @description
    --< Computes the component-wise natural exponentiation of x, e^x for a 
    --< GenFType vector.
    ----------------------------------------------------------------------------
    function Exp is new GFT.Apply_Func_IV_RV(Exp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the natural logarithm of x.
    --<
    --< @description
    --< Computes the natural logarithm of x, which satisfies equation x=e^y, for
    --< a single precision floating point number.
    --<
    --< @param x 
    --< The value 'x'.
    --<
    --< @return 
    --< The result of ln(x).
    ----------------------------------------------------------------------------
    function Log (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Log;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the natural logarithm of x, component-wise.
    --<
    --< @description
    --< Computes the natural logarithm of x, which satisfies equation x=e^y, for
    --< a GenFType vector.
    ----------------------------------------------------------------------------
    function Log is new GFT.Apply_Func_IV_RV(Log);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the binary exponentiation of x, 2^x.
    --<
    --< @description
    --< Computes 2 raised to the x power, 2^x, for a single precision floating
    --< point number.
    --<
    --< @param x
    --< The value 'x'.
    --<
    --< @return
    --< The result of 2^x.
    ----------------------------------------------------------------------------
    function Exp2 (x : in     Vkm_Float) return Vkm_Float is
        (Exp( LN2 * x)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the binary exponentiation of x, component-wise.
    --<
    --< @description
    --< Computes 2 raised to the x power, 2^x, for a GenFType vector.
    ----------------------------------------------------------------------------
    function Exp2 is new GFT.Apply_Func_IV_RV(Exp2);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes log base 2 of x.
    --<
    --< @description
    --< Computes log base 2 of x, finding the value y which satisfies y = 2^x, for
    --< a single precision floating point number.
    --<
    --< @param x 
    --< The value 'x'.
    --<
    --< @returns y = 2^x.
    --<
    --< @error
    --< Results are undefined for x <= 0.
    ----------------------------------------------------------------------------
    function Log2 (x : in     Vkm_Float) return Vkm_Float is
        (Exp(x) / LN2) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes log base 2 of x, component-wise.
    --<
    --< @description
    --< Computes the component-wise log base 2 of x, finding the value y which 
    --< satisfies y = 2^x, for a GenFType vector.
    ----------------------------------------------------------------------------
    function Log2 is new GFT.Apply_Func_IV_RV(Log2);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the square root of x.
    --<
    --< @description
    --< Computes the square root of x for a single-precision floating point 
    --< number.
    --<
    --< @param x 
    --< The value 'x'
    --<
    --< @return 
    --< The result of sqrt(x)
    ----------------------------------------------------------------------------
    function Sqrt (x : in     Vkm_Float ) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Sqrt;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the square root of x.
    --<
    --< @description
    --< Computes the square root of x for a double-precision floating point 
    --< number.
    --<
    --< @param x 
    --< The value 'x'
    --<
    --< @return 
    --< The result of sqrt(x)
    ----------------------------------------------------------------------------
    function Sqrt (x : in     Vkm_Double) return Vkm_Double
        renames Vulkan.Math.Numerics.VKM_DBL_NEF.Sqrt;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the square root of x, component-wise.
    --<
    --< @description
    --< Computes the component-wise square root of x for a GenFType vector.
    ----------------------------------------------------------------------------
    function Sqrt is new GFT.Apply_Func_IV_RV(Sqrt);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the square root of x, component-wise.
    --<
    --< @description
    --< Computes the component-wise square root of x for a GenDType vector.
    ----------------------------------------------------------------------------
    function Sqrt is new GDT.Apply_Func_IV_RV(Sqrt);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the inverse square root of x.
    --<
    --< @description
    --< Computes the inverse square root of x for a single precision floating point
    --< number:
    --<     y = 1/sqrt(x)
    --<
    --< @param x 
    --< The value 'x'
    --<
    --< @return
    --< The inverse square root of x.
    ----------------------------------------------------------------------------
    function Inverse_Sqrt(x : in     Vkm_Float ) return Vkm_Float is
        (1.0 / Sqrt(x)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the inverse square root of x.
    --<
    --< @description
    --< Computes the inverse square root of x for a double precision floating point
    --< number:
    --<     y = 1/sqrt(x)
    --<
    --< @param x 
    --< The value 'x'
    --<
    --< @return
    --< The inverse square root of x.
    ----------------------------------------------------------------------------
    function Inverse_Sqrt(x : in     Vkm_Double) return Vkm_Double is
        (1.0 / Sqrt(x)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the inverse square root of x, component-wise.
    --<
    --< @description
    --< Computes the component-wise inverse square root of x for a GenFType vector.
    ----------------------------------------------------------------------------
    function Inverse_Sqrt is new GFT.Apply_Func_IV_RV(Inverse_Sqrt);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the inverse square root of x, component-wise.
    --<
    --< @description
    --< Computes the component-wise inverse square root of x for a GenDType vector.
    ----------------------------------------------------------------------------
    function Inverse_Sqrt is new GDT.Apply_Func_IV_RV(Inverse_Sqrt);


end Vulkan.Math.Exp;
