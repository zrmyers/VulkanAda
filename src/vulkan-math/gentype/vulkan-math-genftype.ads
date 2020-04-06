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
with Vulkan.Math.GenType;
with Vulkan.Math.GenBType;
with Vulkan.Math.GenIType;
with Vulkan.Math.GenUType;

use Vulkan.Math.GenBType;
use Vulkan.Math.GenUType;
use Vulkan.Math.GenIType;

package Vulkan.Math.GenFType is
    pragma Preelaborate;
    pragma Pure;

    package GFT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Float, Image => Vkm_Float'Image);

    subtype Vkm_GenFType is GFT.Vkm_GenType;

    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    generic
        with function Func(ISF1, ISF2 : in     Vkm_Float;
                           ISB1       : in     Vkm_Bool ) return Vkm_Float;
    function Apply_Func_IVF_IVF_IVB_RVF(IVF1, IVF2 : in     Vkm_GenFType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenFType;

    generic
        with function Func(ISF : in     Vkm_Float) return Vkm_Bool;
    function Apply_Func_IVF_RVB (IVF1 : in     Vkm_GenFType) return Vkm_GenBType;

    generic
        with function Func (ISF : in     Vkm_Float) return Vkm_Int;
    function Apply_Func_IVF_RVI (IVF1 : in     Vkm_GenFType) return Vkm_GenIType;

    generic
        with function Func (ISI : in     Vkm_Int) return Vkm_Float;
    function Apply_Func_IVI_RVF (IVI1 : in     Vkm_GenIType) return Vkm_GenFType;

    generic
        with function Func (ISF : in     Vkm_Float) return Vkm_Uint;
    function Apply_Func_IVF_RVU (IVF1 : in     Vkm_GenFType) return Vkm_GenUType;

    generic
        with function Func (ISU : in     Vkm_Uint) return Vkm_Float;
    function Apply_Func_IVU_RVF (IVU1 : in     Vkm_GenUType) return Vkm_GenFType;

    generic
        with function Func (ISF : in     Vkm_Float;
                            OSI :    out Vkm_Int) return Vkm_Float;
    function Apply_Func_IVF_OVI_RVF(IVF : in     Vkm_GenFType;
                                    OVI :    out Vkm_GenIType) return Vkm_GenFType;

    generic
        with function Func (ISF : in     Vkm_Float;
                            ISI : in     Vkm_Int) return Vkm_Float;
    function Apply_Func_IVF_IVI_RVF(IVF : in     Vkm_GenFType;
                                    IVI : in     Vkm_GenIType) return Vkm_GenFType;

    generic
        with function Func (ISF1 : in     Vkm_Float;
                            ISF2 : in     Vkm_Float) return Vkm_Bool;
    function Apply_Func_IVF_IVF_RVB(IVF1, IVF2 : in     Vkm_GenFType) return Vkm_GenBType;

end Vulkan.Math.GenFType;
