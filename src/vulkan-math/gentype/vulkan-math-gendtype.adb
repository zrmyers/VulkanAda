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
-- This package describes a generic Double Precision Float Vulkan Math type.
--------------------------------------------------------------------------------
package body Vulkan.Math.GenDType is


    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------


    function Apply_Func_IVD_IVD_IVB_RVD(IVD1, IVD2 : in     Vkm_GenDType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenDType is
        Result : Vkm_GenDType := IVD1;

    begin
        for I in Vkm_Indices'First .. To_Indices(IVD1.Length) loop
            Result.data(I) := Func(IVD1.data(I), IVD2.data(I), IVB1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVD_IVD_IVB_RVD;

    function Apply_Func_IVD_RVB(IVD1 : in     Vkm_GenDType) return Vkm_GenBType is

        Result : Vkm_GenBType := (Last_Index => IVD1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVD1.Last_Index loop
            Result.data(I) := Func(IVD1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVD_RVB;

    function Apply_Func_IVD_OVI_RVD(IVD : in     Vkm_GenDType;
                                    OVI :    out Vkm_GenIType) return Vkm_GenDType is

        Result : Vkm_GenDType := (Last_Index => IVD.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVD.Last_Index loop
            Result.data(I) := Func(IVD.data(I),OVI.data(I));
        end loop;
        return Result;
    end Apply_Func_IVD_OVI_RVD;

    function Apply_Func_IVD_IVI_RVD(IVD : in     Vkm_GenDType;
                                    IVI : in     Vkm_GenIType) return Vkm_GenDType is

        Result : Vkm_GenDType := (Last_Index => IVD.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVD.Last_Index loop
            Result.data(I) := Func(IVD.data(I),IVI.data(I));
        end loop;
        return Result;
    end Apply_Func_IVD_IVI_RVD;


    function Apply_Func_IVD_IVD_RVB(IVD1, IVD2 : in     Vkm_GenDType) return Vkm_GenBType is
        Result : Vkm_GenBType := (Last_Index => IVD1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVD1.Last_Index loop
            Result.data(I) := Func(IVD1.data(I),IVD2.data(I));
        end loop;
        return Result;
    end Apply_Func_IVD_IVD_RVB;


end Vulkan.Math.GenDType;
