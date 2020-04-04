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

package Vulkan.Math.Trig is
    pragma Preelaborate;
    pragma Pure;
 
    DEGREES_TO_RADIANS : constant := PI / 180.0;
    RADIANS_TO_DEGREES : constant := 180.0 / PI;
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Convert a generic floating point value in degrees to radians.
    --
    -- @param[in]     degrees The value to convert to radians.
    -- 
    -- @return The result in radians.
    ----------------------------------------------------------------------------
    function Radians (degrees : in     Vkm_Float) return Vkm_Float is
        (degrees * DEGREES_TO_RADIANS) with Inline;
    function Radians is new GFT.Apply_Func_IV_RV(Radians);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Convert a generic floating point value in degrees to radians.
    -- 
    -- @param[in]     radians The value to convert to degrees.
    --
    -- @return The result in degrees.
    ----------------------------------------------------------------------------
    function Degrees (radians : in     Vkm_Float) return Vkm_Float is
        (radians * RADIANS_TO_DEGREES) with Inline;
    function Degrees is new GFT.Apply_Func_IV_RV(Degrees);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate sin of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The sin of the angle.
    ----------------------------------------------------------------------------
    function Sin (angle : in     Vkm_Float) return Vkm_Float 
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Sin;
    function Sin is new GFT.Apply_Func_IV_RV(Sin);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate cos of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The cos of the angle.
    ----------------------------------------------------------------------------
    function Cos (angle : in     Vkm_Float) return Vkm_Float 
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Cos;
    function Cos is new GFT.Apply_Func_IV_RV(Cos);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate tan of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The tan of the angle.
    ----------------------------------------------------------------------------
    function Tan (angle : in     Vkm_Float) return Vkm_Float 
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Tan;
    function Tan is new GFT.Apply_Func_IV_RV(Tan);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate arc sin of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The asin of the angle.
    ----------------------------------------------------------------------------
    function Asin (angle : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arcsin;
    function Asin is new GFT.Apply_Func_IV_RV(Asin);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate arc cos of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The arc cos of the angle.
    ----------------------------------------------------------------------------
    function Acos (angle : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arccos;
    function Acos is new GFT.Apply_Func_IV_RV(Acos);
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Arc tangent. 
    --
    -- @param[in]     y The 'y' value for the function. Sign is used to determine
    --                  which quadrant the angle is in.
    -- @param[in]     x The 'x' value for the function. Sign is used to determine
    --                  which quadrant the angle is in.
    --
    -- @return An angle tangent to y/x. Range is [-PI,PI].
    --
    -- @error
    -- Results are undefined if x and y are both 0.
    ----------------------------------------------------------------------------
    function Atan (y : in     Vkm_Float;
                   x : in     Vkm_Float := 1.0) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arctan;
    function Atan is new GFT.Apply_Func_IV_IV_RV(Atan);
    
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Arc tangent. 
    --
    -- @param[in]     y_over_x The ratio y/x.
    --
    -- @return An angle tangent to y/x. Range is [-PI/2,PI/2].
    --
    -- @error
    -- Results are undefined if x and y are both 0.
    ----------------------------------------------------------------------------
    function Atan (y_over_x : in     Vkm_Float) return Vkm_Float is
        (Atan(y => y_over_x)) with Inline;
    function Atan is new GFT.Apply_Func_IV_RV(Atan);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate the hyperbolic sin function (e^x - e^-x)/2
    --
    -- @param[in]     x The value of 'x'.
    --
    -- @return The hyperbolic sin of 'x'.
    ----------------------------------------------------------------------------
    function Sinh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Sinh;
    function Sinh is new GFT.Apply_Func_IV_RV(Sinh);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate the hyperbolic cos function (e^x + e^-x)/2.
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return The hyperbolic cos of 'x'.
    ----------------------------------------------------------------------------
    function Cosh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Cosh;
    function Cosh is new GFT.Apply_Func_IV_RV(Cosh);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate hyperbolic tan function sinh(x)/cosh(x).
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return The hyperboic tan of 'x'.
    ----------------------------------------------------------------------------
    function Tanh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Tanh;
    function Tanh is new GFT.Apply_Func_IV_RV(Tanh);
    
   
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate hyperbolic arc sin of x.
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return The inverse of sinh.
    ----------------------------------------------------------------------------
    function Asinh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arcsinh;
    function Asinh is new GFT.Apply_Func_IV_RV(Asinh);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate hyperbolic arc cos of x
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return The non-negative inverse of cosh.
    --
    -- @error
    -- Results are undefined if x < 1.
    ----------------------------------------------------------------------------
    function Acosh (x : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arccosh;
    function Acosh is new GFT.Apply_Func_IV_RV(Acosh);
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Hyperbolic arc tangent. 
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return inverse of tanh(x)
    --
    -- @error
    -- Results are undefined if x >= 0.
    ----------------------------------------------------------------------------
    function Atanh (x  : in     Vkm_Float) return Vkm_Float
        renames Vulkan.Math.Numerics.VKM_FLT_NEF.Arctanh;
    function Atanh is new GFT.Apply_Func_IV_RV(Atanh);
    
    
end Vulkan.Math.Trig;
