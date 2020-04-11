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
        for I in Vkm_Indices'First .. To_Vkm_Indices(IVF1.Length) loop
            Result.data(I) := Func(IVF1.data(I), IVF2.data(I), IVB1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVF_IVF_IVB_RVF;


    function Apply_Func_IVF_RVB(IVF1 : in     Vkm_GenFType) return Vkm_GenBType is

        Result : Vkm_GenBType := (Last_Index => IVF1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVF1.Last_Index loop
            Result.data(I) := Func(IVF1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVF_RVB;


    function Apply_Func_IVF_RVI (IVF1 : in     Vkm_GenFType) return Vkm_GenIType is

        Result : Vkm_GenIType := (Last_Index => IVF1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVF1.Last_Index loop
            Result.data(I) := Func(IVF1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVF_RVI;

    function Apply_Func_IVI_RVF (IVI1 : in     Vkm_GenIType) return Vkm_GenFType is

        Result : Vkm_GenFType := (Last_Index => IVI1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVI1.Last_Index loop
            Result.data(I) := Func(IVI1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVI_RVF;

    function Apply_Func_IVF_RVU (IVF1 : in     Vkm_GenFType) return Vkm_GenUType is

        Result : Vkm_GenUType := (Last_Index => IVF1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVF1.Last_Index loop
            Result.data(I) := Func(IVF1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVF_RVU;

    function Apply_Func_IVU_RVF (IVU1 : in     Vkm_GenUType) return Vkm_GenFType is
        Result : Vkm_GenFType := (Last_Index => IVU1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVU1.Last_Index loop
            Result.data(I) := Func(IVU1.data(I));
        end loop;
        return Result;
    end Apply_Func_IVU_RVF;

    function Apply_Func_IVF_OVI_RVF(IVF : in     Vkm_GenFType;
                                    OVI :    out Vkm_GenIType) return Vkm_GenFType is
        Result : Vkm_GenFType := (Last_Index => IVF.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVF.Last_Index loop
            Result.data(I) := Func(IVF.data(I),OVI.data(I));
        end loop;
        return Result;
    end Apply_Func_IVF_OVI_RVF;

    function Apply_Func_IVF_IVI_RVF(IVF : in     Vkm_GenFType;
                                    IVI : in     Vkm_GenIType) return Vkm_GenFType is
        Result : Vkm_GenFType := (Last_Index => IVF.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVF.Last_Index loop
            Result.data(I) := Func(IVF.data(I),IVI.data(I));
        end loop;
        return Result;
    end Apply_Func_IVF_IVI_RVF;


    function Apply_Func_IVF_IVF_RVB(IVF1, IVF2 : in     Vkm_GenFType) return Vkm_GenBType is
        Result : Vkm_GenBType := (Last_Index => IVF1.Last_Index, others => <>);
    begin
        for I in Vkm_Indices'First .. IVF1.Last_Index loop
            Result.data(I) := Func(IVF1.data(I), IVF2.data(I));
        end loop;
        return Result;
    end Apply_Func_IVF_IVF_RVB;

end Vulkan.Math.GenFType;
