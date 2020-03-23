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
package body Vulkan.Math.Numerics is
                          
    -- Get the sign of the integer. +1 if positive, -1 if negative
    function Sign         (x   : in     Numeric_Type) return Numeric_Type is
    
    Result : Numeric_Type := Zero;
    
    begin
        
        if x < Zero then
            Result := Minus;
        elsif x > Zero then
            Result := Plus;
        end if;
        
        return Result;
    end Sign;
    
    -- Retrun the smaller of the two numbers.
    function Min          (x      : in     Numeric_Type;
                           y      : in     Numeric_Type) return Numeric_Type is
    begin
        return Mix(x,y,x < y);
    end Min;
                           
    -- Return the larger of the two numbers.
    function Max          (x      : in     Numeric_Type;
                           y      : in     Numeric_Type) return Numeric_Type is
    begin
        return Mix(x,y,x > y);
    end Max;
                           
    -- Returns min(max(x,minVal),maxVal). Results are undefined if minVal > maxVal.
    function Clamp        (x      : in     Numeric_Type;
                           minVal : in     Numeric_Type;
                           maxVal : in     Numeric_Type) return Numeric_Type is
    begin
        if minVal > maxVal then
            raise UNDEFINED_RESULT with "minVal is greater than maxVal";
        else
            return min(max(x,minVal),maxVal);
        end if;
    end Clamp;
    
    -- If a is true, return x. Otherwise return y.
    function Mix          (x      : in     Numeric_Type;
                           y      : in     Numeric_Type;
                           a      : in     Vkm_Bool)     return Numeric_Type is
                           
        Result : Numeric_Type := x;
        
    begin
        if not a then
            Result := y;
        end if;
        return Result;
    end Mix;
    
    -- If x is 0 return false.  Otherwise return true.
    function To_Vkm_Bool  (x      : in     Numeric_Type) return Vkm_Bool is
    
        Result : Vkm_Bool := False;
    begin
        if x /= Zero then
            Result := True;
        end if
        return Result;
    end To_Vkm_Bool;
        

end Vulkan.Math.Numerics;
