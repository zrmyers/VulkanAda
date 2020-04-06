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
-- This package describes a generic Unsigned Integer Vulkan Math type.
--------------------------------------------------------------------------------
package body Vulkan.Math.GenIType is

    function Apply_Func_IVI_IVI_IVB_RVI(IVI1, IVI2 : in     Vkm_GenIType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenIType is
        Result : Vkm_GenIType := IVI1;
    begin
        for I in Vkm_Indices'First .. Result.Last_Index loop
            Result.data(I) := Func(IVI1.data(I), IVI2.data(I), IVB1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVI_IVI_IVB_RVI;


    function Apply_Func_IVI_IVI_RVB(IVI1, IVI2 : in     Vkm_GenIType) return Vkm_GenBType is
        Result : Vkm_GenBType := (Last_Index => IVI1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVI1.Last_Index loop
            Result.data(I) := Func(IVI1.data(I),IVI2.data(I));
        end loop;
        return Result;
    end Apply_Func_IVI_IVI_RVB;


end Vulkan.Math.GenIType;
