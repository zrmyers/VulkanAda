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
package body Vulkan.Math.GenUType is


    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------


    function Apply_Func_IVU_IVU_IVB_RVU(IVU1, IVU2 : in     Vkm_GenUType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenUType is
        Result : Vkm_GenUType := IVU1;
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(Result.Length) loop
            Result.data(I) := Func(IVU1.data(I), IVU2.data(I), IVB1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVU_IVU_IVB_RVU;

    function Apply_Func_IVU_IVU_RVB(IVU1, IVU2 : in     Vkm_GenUType) return Vkm_GenBType is
        Result : Vkm_GenBType := (Last_Index => IVU1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVU1.Last_Index loop
            Result.data(I) := Func(IVU1.data(I), IVU2.data(I));
        end loop;
        return Result;
    end Apply_Func_IVU_IVU_RVB;

end Vulkan.Math.GenUType;
