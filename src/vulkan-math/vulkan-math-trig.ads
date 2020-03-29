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

use Vulkan.Math.GenFType;

package Vulkan.Math.Trig is
    pragma Preelaborate;
    pragma Pure;
    
 
    ----------------------------------------------------------------------------
    -- @brief
    -- Convert a generic floating point value in degrees to radians.
    --
    -- @param[in]     degrees The value to convert to radians.
    -- 
    -- @return The result in radians.
    ----------------------------------------------------------------------------     
    function Radians (degrees : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Convert a generic floating point value in degrees to radians.
    -- 
    -- @param[in]     radians The value to convert to degrees.
    --
    -- @return The result in degrees.
    ----------------------------------------------------------------------------
    function Degrees (radians : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate sin of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The sin of the angle.
    ----------------------------------------------------------------------------
    function Sin (angle : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate cos of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The cos of the angle.
    ----------------------------------------------------------------------------
    function Cos (angle : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate tan of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The tan of the angle.
    ----------------------------------------------------------------------------
    function Tan (angle : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate arc sin of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The asin of the angle.
    ----------------------------------------------------------------------------
    function Asin (angle : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate arc cos of angle.
    --
    -- @param[in]     angle The value of the angle.
    --
    -- @return The arc cos of the angle.
    ----------------------------------------------------------------------------
    function Acos (angle : in     Vkm_GenFType) return Vkm_GenFType;
    
    
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
    function Atan (y, x  : in     Vkm_GenFType) return Vkm_GenFType;
    
    
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
    function Atan (y_over_x : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate the hyperbolic sin function (e^x - e^-x)/2
    --
    -- @param[in]     x The value of 'x'.
    --
    -- @return The hyperbolic sin of 'x'.
    ----------------------------------------------------------------------------
    function Sinh (x : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate the hyperbolic cos function (e^x + e^-x)/2.
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return The hyperbolic cos of 'x'.
    ----------------------------------------------------------------------------
    function Cosh (x : in     Vkm_GenFType) return Vkm_GenFType;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate hyperbolic tan function sinh(x)/cosh(x).
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return The hyperboic tan of 'x'.
    ----------------------------------------------------------------------------
    function Tanh (x : in     Vkm_GenFType) return Vkm_GenFType;
    
   
    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate hyperbolic arc sin of x.
    --
    -- @param[in]     x The value 'x'.
    --
    -- @return The inverse of sinh.
    ----------------------------------------------------------------------------
    function Asinh (x : in     Vkm_GenFType) return Vkm_GenFType;
    
    
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
    function Acosh (x : in     Vkm_GenFType) return Vkm_GenFType;
    
    
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
    function Atanh (x  : in     Vkm_GenFType) return Vkm_GenFType;
    
    
end Vulkan.Math.Trig;
