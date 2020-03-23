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
-- This package provides a Boolean math type that can be used with Vulkan 
-- Shaders.
--
--------------------------------------------------------------------------------
with Vulkan.Math.Bool;

use Vulkan.Math.Bool;
    
generic
    type Numeric_Type is private;
    Zero  : in Numeric_Type;
    Plus  : in Numeric_Type;
    Minus : in Numeric_Type;
    with function "<" (Left, Right : in     Numeric_Type) return Vkm_Bool;
    with function ">" (Left, Right : in     Numeric_Type) return Vkm_Bool;
package Vulkan.Math.Numerics is
    pragma Preelaborate;
    pragma Pure;
                     
    -- This exception is thrown if an operation yields undefined behavior.
    UNDEFINED_RESULT : exception;
    
    -- Get the sign of the integer. +1 if positive, -1 if negative
    function Sign         (x      : in     Numeric_Type) return Numeric_Type;
    
    -- Retrun the smaller of the two numbers.
    function Min          (x      : in     Numeric_Type;
                           y      : in     Numeric_Type) return Numeric_Type;
                           
    -- Return the larger of the two numbers.
    function Max          (x      : in     Numeric_Type;
                           y      : in     Numeric_Type) return Numeric_Type;
                           
    -- Returns min(max(x,minVal),maxVal). Results are undefined if minVal > maxVal.
    function Clamp        (x      : in     Numeric_Type;
                           minVal : in     Numeric_Type;
                           maxVal : in     Numeric_Type) return Numeric_Type;
                           
    -- If a is true, return x. Otherwise return y.
    function Mix          (x      : in     Numeric_Type;
                           y      : in     Numeric_Type;
                           a      : in     Vkm_Bool)      return Numeric_Type;
                           
    function To_Vkm_Bool  (x      : in     Numeric_Type) return Vkm_Bool;

end Vulkan.Math.Numerics;
