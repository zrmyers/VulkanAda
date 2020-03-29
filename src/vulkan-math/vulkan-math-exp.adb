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
with Vulkan.Math.Numerics;
use Vulkan.Math.Numerics;

package body Vulkan.Math.Exp is

    -- Ln(2)
    LN2 : constant := 0.69314_71805_59945_30941_72321_21458_18;

    ----------------------------------------------------------------------------


    function "**" (x, y : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Power is new GFT.Apply_Func_IV_IV_RV(VKM_FLT_NEF."**");
    begin
        return Apply_Power(x, y);
    end "**";


    ----------------------------------------------------------------------------


    function Exp (x : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Exp is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Exp);
    begin
        return Apply_Exp(x);
    end Exp;


    ----------------------------------------------------------------------------


    function Log (x : in     Vkm_GenFType) return Vkm_GenFType is
        function Apply_Log is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Log);
    begin
        return Apply_Log(x);
    end Log;


    ----------------------------------------------------------------------------


    function Exp2 (x : in     Vkm_GenFType) return Vkm_GenFType is

        function Local_Exp2 (x : in     Vkm_Float) return Vkm_Float is
            (VKM_FLT_NEF.Exp(X => x * LN2)) with Inline;

        function Apply_Exp2 is new GFT.Apply_Func_IV_RV(Local_Exp2);
    begin
        return Apply_Exp2(x);
    end Exp2;


    ----------------------------------------------------------------------------


    function Log2 (x : in     Vkm_GenFType) return Vkm_GenFType is

        function Calculate_Log2 (x : in     Vkm_Float) return Vkm_Float is
            (VKM_FLT_NEF.Exp(X => x) / LN2) with Inline;

        function Apply_Log2 is new GFT.Apply_Func_IV_RV(Calculate_Log2);

    begin
        return Apply_Log2(x);
    end Log2;


    ----------------------------------------------------------------------------


    function Sqrt(x : in     Vkm_GenFType ) return Vkm_GenFType is
        function Apply_Sqrt is new GFT.Apply_Func_IV_RV(VKM_FLT_NEF.Sqrt);
    begin
        return Apply_Sqrt(x);
    end Sqrt;

    -- Variety for doubles
    function Sqrt(x : in     Vkm_GenDType) return Vkm_GenDType is
        function Apply_Sqrt is new GDT.Apply_Func_IV_RV(VKM_DBL_NEF.Sqrt);
    begin
        return Apply_Sqrt(x);
    end Sqrt;


    ----------------------------------------------------------------------------


    function Inverse_Sqrt(x : in     Vkm_GenFType ) return Vkm_GenFType is

        function Calculate_Inverse_Sqrt(x : in     Vkm_Float) return Vkm_Float is
            (1.0 / VKM_FLT_NEF.Sqrt(X => x)) with Inline;

        function Apply_Inverse_Sqrt is new GFT.Apply_Func_IV_RV(Calculate_Inverse_Sqrt);
    begin
        return Apply_Inverse_Sqrt(x);
    end;

    function Inverse_Sqrt(x : in     Vkm_GenDType) return Vkm_GenDType is

        function Calculate_Inverse_Sqrt(x : in     Vkm_Double) return Vkm_Double is
            (1.0 / VKM_DBL_NEF.Sqrt(X => x)) with Inline;

        function Apply_Inverse_Sqrt is new GDT.Apply_Func_IV_RV(Calculate_Inverse_Sqrt);
    begin
        return Apply_Inverse_Sqrt(x);
    end Inverse_Sqrt;

end Vulkan.Math.Exp;
