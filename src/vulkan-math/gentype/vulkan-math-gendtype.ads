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
with Vulkan.Math.GenType;
with Vulkan.Math.GenBType;
with Vulkan.Math.GenIType;

use Vulkan.Math.GenBType;
use Vulkan.Math.GenIType;

package Vulkan.Math.GenDType is
    pragma Preelaborate;
    pragma Pure;

    package GDT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Double, Image => Vkm_Double'Image);

    subtype Vkm_GenDType is GDT.Vkm_GenType;


    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    generic
        with function Func(ISD1, ISD2 : in     Vkm_Double;
                           ISB1       : in     Vkm_Bool ) return Vkm_Double;
    function Apply_Func_IVD_IVD_IVB_RVD(IVD1, IVD2 : in     Vkm_GenDType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenDType;

    generic
        with function Func(ISD : in     Vkm_Double) return Vkm_Bool;
    function Apply_Func_IVD_RVB(IVD1 : in     Vkm_GenDType) return Vkm_GenBType;

    generic
        with function Func (ISD : in     Vkm_Double;
                            ISI :    out Vkm_Int) return Vkm_Double;
    function Apply_Func_IVD_OVI_RVD(IVD : in     Vkm_GenDType;
                                    OVI :    out Vkm_GenIType) return Vkm_GenDType;

    generic
        with function Func (ISD : in     Vkm_Double;
                            ISI : in     Vkm_Int) return Vkm_Double;
    function Apply_Func_IVD_IVI_RVD(IVD : in     Vkm_GenDType;
                                    IVI : in     Vkm_GenIType) return Vkm_GenDType;
end Vulkan.Math.GenDType;
