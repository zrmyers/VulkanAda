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
-- This package describes a generic Single Precision Floating Point Vulkan Math type.
--------------------------------------------------------------------------------
package body Vulkan.Math.GenFType is


    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------


    function Apply_Func_IVF_IVF_IVB_RVF(IVF1, IVF2 : in     Vkm_GenFType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenFType is
        Result : Vkm_GenFType := IVF1;

    begin
        for I in Vkm_Indices'First .. To_Indices(IVF1.Length) loop
            Result.data(I) := Func(IVF1.data(I), IVF2.data(I), IVB1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVF_IVF_IVB_RVF;



end Vulkan.Math.GenFType;
