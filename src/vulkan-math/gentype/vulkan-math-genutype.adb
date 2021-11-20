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
        result : Vkm_GenUType := (last_index => IVU1.last_index, others => <>);
    begin
        for index in result.data'Range loop
            result.data(index) := Func(IVU1.data(index),
                                       IVU2.data(index),
                                       IVB1.data(index));
        end loop;
        return Result;
    end Apply_Func_IVU_IVU_IVB_RVU;


    ----------------------------------------------------------------------------


    function Apply_Func_IVU_IVU_RVB(
        IVU1, IVU2 : in     Vkm_GenUType) return Vkm_GenBType is

        result : Vkm_GenBType := (last_index => IVU1.last_index, others => <>);
    begin
        for index in result.data'Range loop
            result.data(index) := Func(IVU1.data(index), IVU2.data(index));
        end loop;
        return result;
    end Apply_Func_IVU_IVU_RVB;


    ----------------------------------------------------------------------------


    function Apply_Func_IVU_ISI_ISI_RVU(
        IVU1       : in     Vkm_GenUType;
        ISI1, ISI2 : in     Vkm_Int     ) return Vkm_GenUType is

        result : Vkm_GenUType := (last_index => IVU1.last_index, others => <>);
    begin
        for index in result.data'Range loop
            result.data(index) := Func(IVU1.data(index), ISI1, ISI2);
        end loop;
        return result;
    end Apply_Func_IVU_ISI_ISI_RVU;


    ----------------------------------------------------------------------------


    function Apply_Func_IVU_IVU_ISI_ISI_RVU(
        IVU1, IVU2 : in     Vkm_GenUType;
        ISI1, ISI2 : in     Vkm_Int     ) return Vkm_GenUType is
        
        result : Vkm_GenUType := (last_index => IVU1.last_index, others => <>);
    begin
        for index in result.data'Range loop
            result.data(index) := Func(IVU1.data(index),
                                       IVU2.data(index),
                                       ISI1, ISI2);
        end loop;
        return result;
    end Apply_Func_IVU_IVU_ISI_ISI_RVU;


end Vulkan.Math.GenUType;
