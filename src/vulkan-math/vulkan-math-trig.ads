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
-- This package provides a mapping of the GLSL trigonometric functions to Ada's
-- Numerics packages.
--------------------------------------------------------------------------------
with Vulkan.Math.GenFType;
with Vulkan.Math.Numerics;

use Vulkan.Math.GenFType;

--------------------------------------------------------------------------------
--< @group Vulkan Math Functions
--------------------------------------------------------------------------------
--< @summary
--< This package provides GLSL Trigonometry Built-in functions.
--<
--< @description
--< All trigonometry functions operate component-wise on vectors.
--------------------------------------------------------------------------------
package Vulkan.Math.Trig is
    pragma Preelaborate;
    pragma Pure;

    --< Constant for converted degrees to radians.
    DEGREES_TO_RADIANS : constant := PI / 180.0;
    
    --< Constant for converting radians to degrees.
    RADIANS_TO_DEGREES : constant := 180.0 / PI;
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Convert Vkm_Float angle in degrees to radians.
    --<
    --< @description
    --< Convert a Vkm_Float angle in degrees to radians.
    --<
    --< @param degrees 
    --< The value to convert to radians.
    --< 
    --< @return 
    --< The result in radians.
    ----------------------------------------------------------------------------
    function Radians (degrees : in     Vkm_Float) return Vkm_Float is
        (degrees * DEGREES_TO_RADIANS) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert Vkm_GenFType angle from degrees to radians.
    --<
    --< @description
    --< Convert Vkm_GenFType angle from degrees to radians.
    ----------------------------------------------------------------------------
    function Radians is new GFT.Apply_Func_IV_RV(Radians);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Convert Vkm_Float angle in radians to degrees.
    --<
    --< @description
    --< Convert Vkm_Float angle in radians to degrees.
    --< 
    --< @param radians 
    --< The value to convert to degrees.
    --<
    --< @return 
    --< The result in degrees.
    ----------------------------------------------------------------------------
    function Degrees (radians : in     Vkm_Float) return Vkm_Float is
        (radians * RADIANS_TO_DEGREES) with Inline;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Convert Vkm_GenFType angle in radians to degrees.
    --<
    --< @description
    --< Convert Vkm_GenFType angle in radians to degrees.
    ----------------------------------------------------------------------------
    function Degrees is new GFT.Apply_Func_IV_RV(Degrees);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate sin of Vkm_Float angle.
    --< 
    --< @description
    --< Calculate sin of Vkm_Float angle.
    --<
    --< @param angle 
    --< The value of the angle.
    --<
    --< @return 
    --< The sin of the angle.
    ----------------------------------------------------------------------------
    function Sin (angle : in     Vkm_Float) return Vkm_Float 
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Sin;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate sin of Vkm_GenFType angle.
    --< 
    --< @description
    --< Calculate sin of Vkm_GenFType angle.
    ----------------------------------------------------------------------------
    function Sin is new GFT.Apply_Func_IV_RV(Sin);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate cos of Vkm_Float angle.
    --<
    --< @description
    --< Calculate cos of Vkm_Float angle.
    --<
    --< @param angle 
    --< The value of the angle.
    --<
    --< @return 
    --< The cos of the angle.
    ----------------------------------------------------------------------------
    function Cos (angle : in     Vkm_Float) return Vkm_Float 
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Cos;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate cos of Vkm_GenFType angle.
    --<
    --< @description
    --< Calculate cos of Vkm_GenFType angle.
    ----------------------------------------------------------------------------
    function Cos is new GFT.Apply_Func_IV_RV(Cos);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate tan of Vkm_Float angle.
    --<
    --< @description
    --< Calculate tan of Vkm_Float angle.
    --<
    --< @param angle 
    --< The value of the angle.
    --<
    --< @return 
    --< The tan of the angle.
    ----------------------------------------------------------------------------
    function Tan (angle : in     Vkm_Float) return Vkm_Float 
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Tan;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate tan of Vkm_GenFType angle.
    --<
    --< @description
    --< Calculate tan of Vkm_GenFType angle.
    ----------------------------------------------------------------------------
    function Tan is new GFT.Apply_Func_IV_RV(Tan);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate arc sin of Vkm_Float angle.
    --<
    --< @description
    --< Calculate arc sin of Vkm_Float angle.
    --<
    --< @param angle 
    --< The value of the angle.
    --<
    --< @return 
    --< The asin of the angle.
    ----------------------------------------------------------------------------
    function Asin (angle : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arcsin;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate arc sin of Vkm_GenFType angle.
    --<
    --< @description
    --< Calculate arc sin of Vkm_GenFType angle.
    ----------------------------------------------------------------------------
    function Asin is new GFT.Apply_Func_IV_RV(Asin);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate arc cos of Vkm_Float angle.
    --<
    --< @description
    --< Calculate arc cos of Vkm_Float angle.
    --<
    --< @param angle 
    --< The value of the angle.
    --<
    --< @return 
    --< The arc cos of the angle.
    ----------------------------------------------------------------------------
    function Acos (angle : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arccos;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate arc cos of Vkm_GenFType angle.
    --<
    --< @description
    --< Calculate arc cos of Vkm_GenFType angle.
    ----------------------------------------------------------------------------
    function Acos is new GFT.Apply_Func_IV_RV(Acos);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Arc tangent of Vkm_Floats x and y.
    --<
    --< @description
    --< Arc tangent of Vkm_Floats x and y.
    --<
    --< @param y 
    --< The 'y' value for the function. Sign is used to determine which quadrant 
    --< the angle is in.
    --<
    --< @param x 
    --< The 'x' value for the function. Sign is used to determine which quadrant 
    --< the angle is in.
    --<
    --< @return 
    --< An angle tangent to y/x. Range is [-PI,PI].
    ----------------------------------------------------------------------------
    function Atan (y : in     Vkm_Float;
                   x : in     Vkm_Float := 1.0) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arctan;


    ----------------------------------------------------------------------------
    --< @summary
    --< Arc tangent of Vkm_GenFType x and y.
    --<
    --< @description
    --< Arc tangent of Vkm_GenFType x and y.
    ----------------------------------------------------------------------------
    function Atan is new GFT.Apply_Func_IV_IV_RV(Atan);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Arc tangent of Vkm_Float x over y.
    --<
    --< @description
    --< Arc tangent of Vkm_Float x over y.
    --<
    --< @param y_over_x 
    --< The ratio y/x.
    --<
    --< @return 
    --< An angle tangent to y/x. Range is [-PI/2,PI/2].
    ----------------------------------------------------------------------------
    function Atan (y_over_x : in     Vkm_Float) return Vkm_Float is
        (Atan(y => y_over_x)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Arc tangent of Vkm_GenFType x over y.
    --<
    --< @description
    --< Arc tangent of Vkm_GenFType x over y.
    ----------------------------------------------------------------------------
    function Atan is new GFT.Apply_Func_IV_RV(Atan);


    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic sin of Vkm_Float angle.
    --<
    --< @description
    --< Calculate the hyperbolic sin function (e^x - e^-x)/2
    --<
    --< @param x 
    --< The value of 'x'.
    --<
    --< @return 
    --< The hyperbolic sin of 'x'.
    ----------------------------------------------------------------------------
    function Sinh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Sinh;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic sin of Vkm_GenFType angle.
    --<
    --< @description
    --< Calculate the hyperbolic sin function (e^x - e^-x)/2
    ----------------------------------------------------------------------------
    function Sinh is new GFT.Apply_Func_IV_RV(Sinh);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic cos of Vkm_Float angle.
    --<
    --< @description
    --< Calculate the hyperbolic cos function (e^x + e^-x)/2.
    --<
    --< @param x 
    --< The value 'x'.
    --<
    --< @return 
    --< The hyperbolic cos of 'x'.
    ----------------------------------------------------------------------------
    function Cosh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Cosh;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic cos of Vkm_GenFType angle.
    --<
    --< @description
    --< Calculate the hyperbolic cos function (e^x + e^-x)/2.
    ----------------------------------------------------------------------------
    function Cosh is new GFT.Apply_Func_IV_RV(Cosh);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic tan of Vkm_Float x.
    --<
    --< @description
    --< Calculate hyperbolic tan function sinh(x)/cosh(x).
    --<
    --< @param x 
    --< The value 'x'.
    --<
    --< @return 
    --< The hyperboic tan of 'x'.
    ----------------------------------------------------------------------------
    function Tanh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Tanh;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic tan of Vkm_GenFType x.
    --<
    --< @description
    --< Calculate hyperbolic tan function sinh(x)/cosh(x).
    ----------------------------------------------------------------------------
    function Tanh is new GFT.Apply_Func_IV_RV(Tanh);
    
   
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic arc sin of Vkm_Float x.
    --<
    --< @description
    --< Calculate hyperbolic arc sin of Vkm_Float x.
    --<
    --< @param x 
    --< The value 'x'.
    --<
    --< @return 
    --< The inverse of sinh.
    ----------------------------------------------------------------------------
    function Asinh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arcsinh;
    
   
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic arc sin of Vkm_GenFType x.
    --<
    --< @description
    --< Calculate hyperbolic arc sin of Vkm_GenFType x.
    ----------------------------------------------------------------------------
    function Asinh is new GFT.Apply_Func_IV_RV(Asinh);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic arc cos of Vkm_Float x.
    --<
    --< @description
    --< Calculate hyperbolic arc cos of x. Results are undefined if x < 1.
    --<
    --< @param x The value 'x'.
    --<
    --< @return 
    --< The non-negative inverse of cosh.
    ----------------------------------------------------------------------------
    function Acosh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arccosh;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate hyperbolic arc cos of Vkm_GenFType x.
    --<
    --< @description
    --< Calculate hyperbolic arc cos of x. Results are undefined if x < 1.
    ----------------------------------------------------------------------------
    function Acosh is new GFT.Apply_Func_IV_RV(Acosh);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate Hyperbolic arc tangent of Vkm_Float x.
    --<
    --< @description
    --< Hyperbolic arc tangent. Results are undefined if x >= 0.
    --<
    --< @param x 
    --< The value 'x'.
    --<
    --< @return inverse of tanh(x)
    ----------------------------------------------------------------------------
    function Atanh (x  : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arctanh;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Calculate Hyperbolic arc tangent of Vkm_GenFType x.
    --<
    --< @description
    --< Hyperbolic arc tangent. Results are undefined if x >= 0.
    ----------------------------------------------------------------------------
    function Atanh is new GFT.Apply_Func_IV_RV(Atanh);
    
    
end Vulkan.Math.Trig;
