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


package body Vulkan.Math.GenIType is


    function Apply_Func_IVI_IVI_IVB_RVI(
        IVI1, IVI2 : in     Vkm_GenIType;
        IVB1       : in     Vkm_GenBType) return Vkm_GenIType is
        
        result : Vkm_GenIType := (last_index => IVI1.last_index, others => <>);
    begin
        for index in result.data'Range loop
            result.data(index) := Func(IVI1.data(index), 
                                       IVI2.data(index), 
                                       IVB1.data(index));
        end loop;
        return result;
    end Apply_Func_IVI_IVI_IVB_RVI;


    ----------------------------------------------------------------------------


    function Apply_Func_IVI_IVI_RVB(
        IVI1, IVI2 : in     Vkm_GenIType) return Vkm_GenBType is
        
        result : Vkm_GenBType := (last_index => IVI1.last_index, others => <>);
    begin
        for index in result.data'Range loop
            result.data(index) := Func(IVI1.data(index),IVI2.data(index));
        end loop;
        return result;
    end Apply_Func_IVI_IVI_RVB;


    ----------------------------------------------------------------------------


    function Apply_Func_IVU_RVI(
        IVU1 : in     Vkm_GenUType) return Vkm_GenIType is
        
        result : Vkm_GenIType := (last_index => IVU1.last_index, others => <>);
    begin
        for index in result.data'Range loop
            result.data(index) := Func(IVU1.data(index));
        end loop;
        return result;
    end Apply_Func_IVU_RVI;


end Vulkan.Math.GenIType;
