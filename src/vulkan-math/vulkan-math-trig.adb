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
-- Built-in geometric functions.
--------------------------------------------------------------------------------
with Vulkan.Math.Numerics;
use Vulkan.Math.Numerics;


package body Vulkan.Math.Trig is

    DEGREES_TO_RADIANS : constant := PI / 180.0;
    RADIANS_TO_DEGREES : constant := 180.0 / PI;
    
    ----------------------------------------------------------------------------
    -- Operation Definitions
    ----------------------------------------------------------------------------


    function Radians (degrees : in     Vkm_GenFType) return Vkm_GenFType is
    begin
        return degrees * DEGREES_TO_RADIANS;
    end Radians;


    ----------------------------------------------------------------------------


    function Degrees (radians : in     Vkm_GenFType) return Vkm_GenFType is
    begin
        return radians * RADIANS_TO_DEGREES;
    end Degrees;


    ----------------------------------------------------------------------------
    
    
    function Sin (angle : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Sin is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Sin);
    begin
        return Apply_Sin(A => angle);
    end Sin;
    
    
    ----------------------------------------------------------------------------
    
    
    function Cos (angle : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Cos is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Cos);
    begin
        return Apply_Cos(A => angle);
    end Cos;
    
    
    ----------------------------------------------------------------------------


    function Tan (angle : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Tan is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Tan);
    begin
        return Apply_Tan(A => angle);
    end Tan;
    
    
    ----------------------------------------------------------------------------


    function Asin (angle : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Asin is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Arcsin);
    begin
        return Apply_Asin(A => angle);
    end Asin;
    
    
    ----------------------------------------------------------------------------


    function Acos (angle : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Acos is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Arccos);
    begin
        return Apply_Acos(A => angle);
    end Acos;
    
    
    ----------------------------------------------------------------------------


    function Atan (y, x  : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Atan is new GFT.Apply_Func_IV_IV_RV(VKM_FLT_NEF.Arctan);
    begin
        return Apply_Atan(Left => y, Right => x);
    end Atan;
    
    ----------------------------------------------------------------------------


    function Atan (y_over_x : in     Vkm_GenFType) return Vkm_GenFType is
    
        function Atan_Unary (y_over_x : in     Vkm_Float) return Vkm_Float is
        begin
            return VKM_FLT_NEF.Arctan(y => y_over_x);
        end Atan_Unary;
        
        function Apply_Atan is new GFT.Apply_Func_IV_RV(Atan_Unary);
        
    begin
        return Apply_Atan(y_over_x);
    end Atan;
    

    ----------------------------------------------------------------------------


    function Sinh (x : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Sinh is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Sinh);
    begin
        return Apply_Sinh(x);
    end Sinh;
    
    
    ----------------------------------------------------------------------------
   
   
    function Cosh (x : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Cosh is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Cosh);
    begin
        return Apply_Cosh(x);
    end Cosh;
    
    
    ----------------------------------------------------------------------------
   
   
    function Tanh (x : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Tanh is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Tanh);
    begin
        return Apply_Tanh(x);
    end Tanh;
        
    
    ----------------------------------------------------------------------------
  
  
    function Asinh (x : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Asinh is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Arcsinh);
    begin
        return Apply_Asinh(x);
    end Asinh;
    
    
    ----------------------------------------------------------------------------
    
    
    function Acosh (x : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Acosh is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Arccosh);
    begin
        return Apply_Acosh(x);
    end Acosh;
    
    
    ----------------------------------------------------------------------------
   
   
    function Atanh (x  : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Atanh is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Arctanh);
    begin
        return Apply_Atanh(x);
    end Atanh;
    
    
end Vulkan.Math.Trig;
