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
with Vulkan.Math.GenType;
with Vulkan.Math.GenBType;
with Vulkan.Math.GenIType;

use Vulkan.Math.GenBType;
use Vulkan.Math.GenIType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenType
--------------------------------------------------------------------------------
--< @summary
--< This package describes any length vector of Vkm_Double type.
--<
--< @description
--< Provides an instantiation of the generic GenType  package with a Base_Type of 
--< Vkm_Double. This is used to provide the Vkm_GenDType subtype for the Vulkan Math 
--< library.
--------------------------------------------------------------------------------
package Vulkan.Math.GenDType is
    pragma Preelaborate;
    pragma Pure;

    --< @private
    --< An instance of the generic GenType package, with Vkm_Double as the Base_Type.
    package GDT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Double, Image => Vkm_Double'Image);

    subtype Vkm_GenDType is GDT.Vkm_GenType;


    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function for parameters of Vkm_Double and Vkm_Bool type to GenDType
    --< and GenBType vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenDType vectors and
    --< a GenBType vector returning a GenDType vector.
    --<
    --<    RVD := [Func(IVD1.x, IVD2.x, IVB1.x) ... Func(IVD1.w, IVD2.w, IVB1.w)]
    --<
    --< @param IVD1
    --< The first input GenDType parameter.
    --<
    --< @param IVD2
    --< The second input GenDType parameter.
    --<
    --< @param IVB1
    --< The first input GenBType parameter.
    --<
    --< @return
    --< The result GenDType vector, RVD.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISD1, ISD2 : in     Vkm_Double;
                           ISB1       : in     Vkm_Bool ) return Vkm_Double;
    function Apply_Func_IVD_IVD_IVB_RVD(IVD1, IVD2 : in     Vkm_GenDType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenDType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Double input that returns a VKm_Bool component-wise
    --< to a GenDType vector.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenDType vector returning 
    --< a GenBType vector.
    --<
    --<    RVB := [Func(IVD1.x) ... Func(IVD1.w)]
    --<
    --< @param IVD1
    --< The input GenDType parameter.
    --<
    --< @return
    --< The resulting GenBType vector, RVB.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISD : in     Vkm_Double) return Vkm_Bool;
    function Apply_Func_IVD_RVB(IVD1 : in     Vkm_GenDType) return Vkm_GenBType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Double input and a Vkm_Int outut that returns a Vkm_Double
    --< component-wise to the corresponding vector types.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenDType and GenIType
    --< vector, returning a GenDType vector.
    --<
    --<    RVD := [Func(IVD.x,OVI.x) ... Func(IVD.w,OVI.w)]
    --<
    --< @param IVD
    --< The input GenDType parameter.
    --<
    --< @param OVI
    --< The output GenIType parameter.
    --<
    --< @return
    --< The resulting GenDType vector, RVD.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISD : in     Vkm_Double;
                            ISI :    out Vkm_Int) return Vkm_Double;
    function Apply_Func_IVD_OVI_RVD(IVD : in     Vkm_GenDType;
                                    OVI :    out Vkm_GenIType) return Vkm_GenDType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Double and a Vkm_Int input that returns a Vkm_Double 
    --< component-wise to the corresponding vector types.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenDType and GenIType
    --< vector, returning a GenDType vector.
    --<
    --<    RVD := [Func(IVD.x,IVI.x) ... Func(IVD.w,IVI.w)]
    --<
    --< @param IVD
    --< The input GenDType parameter.
    --<
    --< @param IVI
    --< The input GenIType parameter.
    --<
    --< @return
    --< The resulting GenDType vector, RVD.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISD : in     Vkm_Double;
                            ISI : in     Vkm_Int) return Vkm_Double;
    function Apply_Func_IVD_IVI_RVD(IVD : in     Vkm_GenDType;
                                    IVI : in     Vkm_GenIType) return Vkm_GenDType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on two Vkm_Double inputs that returns a Vkm_Bool component-wise
    --< to two Vkm_GenDType vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenDType vectors, 
    --< returning a GenBType vector.
    --<
    --<    RVB := [Func(IVD1.x,IVD2.x) ... Func(IVD1.w,IVD2.w)]
    --<
    --< @param IVD1
    --< The first input GenDType parameter.
    --<
    --< @param IVD2
    --< The second input GenDType parameter.
    --<
    --< @return
    --< The resulting GenBType vector, RVB.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISD1 : in     Vkm_Double;
                            ISD2 : in     Vkm_Double) return Vkm_Bool;
    function Apply_Func_IVD_IVD_RVB(IVD1, IVD2 : in     Vkm_GenDType) return Vkm_GenBType;


end Vulkan.Math.GenDType;
