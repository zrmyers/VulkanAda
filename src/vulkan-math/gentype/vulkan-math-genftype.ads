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
with Vulkan.Math.GenUType;

use Vulkan.Math.GenBType;
use Vulkan.Math.GenUType;
use Vulkan.Math.GenIType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenType
--------------------------------------------------------------------------------
--< @summary
--< This package describes any length vector of Vkm_Float type.
--<
--< @description
--< Provides an instantiation of the generic GenType  package with a Base_Type of 
--< Vkm_Float. This is used to provide the Vkm_GenFType subtype for the Vulkan Math 
--< library.
--------------------------------------------------------------------------------
package Vulkan.Math.GenFType is
    pragma Preelaborate;
    pragma Pure;

    --< @private
    --< An instance of the generic GenType package, with Vkm_Float as the Base_Type.
    package GFT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Float, Image => Vkm_Float'Image);
        
    --< A subtype of the instantiated Vkm_GenType that represents the GenFType 
    --< described in the GLSL specification.
    subtype Vkm_GenFType is GFT.Vkm_GenType;

    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function for parameters of Vkm_Float and Vkm_Bool type to GenFType
    --< and GenBType vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenFType vectors and
    --< a GenBType vector returning a GenFType vector.
    --<
    --<    RVF := [Func(IVF1.x, IVF2.x, IVB1.x) ... Func(IVF1.w, IVF2.w, IVB1.w)]
    --<
    --< @param IVF1
    --< The first input GenFType parameter.
    --<
    --< @param IVF2
    --< The second input GenFType parameter.
    --<
    --< @param IVB1
    --< The first input GenBType parameter.
    --<
    --< @return
    --< The result GenFType vector, RVF.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISF1, ISF2 : in     Vkm_Float;
                           ISB1       : in     Vkm_Bool ) return Vkm_Float;
    function Apply_Func_IVF_IVF_IVB_RVF(IVF1, IVF2 : in     Vkm_GenFType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenFType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Float input that returns a VKm_Bool component-wise
    --< to a GenFType vector.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenFType vector returning 
    --< a GenBType vector.
    --<
    --<    RVB := [Func(IVF1.x) ... Func(IVF1.w)]
    --<
    --< @param IVF1
    --< The input GenFType parameter.
    --<
    --< @return
    --< The resulting GenBType vector, RVB.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISF : in     Vkm_Float) return Vkm_Bool;
    function Apply_Func_IVF_RVB (IVF1 : in     Vkm_GenFType) return Vkm_GenBType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Float input that returns a Vkm_Int component-wise
    --< to a GenFType vector.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenFType vector returning 
    --< a GenIType vector.
    --<
    --<    RVI := [Func(IVF1.x) ... Func(IVF1.w)]
    --<
    --< @param IVF1
    --< The input GenFType parameter.
    --<
    --< @return
    --< The resulting GenBType vector, RVI.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISF : in     Vkm_Float) return Vkm_Int;
    function Apply_Func_IVF_RVI (IVF1 : in     Vkm_GenFType) return Vkm_GenIType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Int input that returns a Vkm_Float component-wise
    --< to a GenIType vector.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenIType vector returning 
    --< a GenFType vector.
    --<
    --<    RVF := [Func(IVI1.x) ... Func(IVI1.w)]
    --<
    --< @param IVI1
    --< The input GenIType parameter.
    --<
    --< @return
    --< The resulting GenFType vector, RVF.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISI : in     Vkm_Int) return Vkm_Float;
    function Apply_Func_IVI_RVF (IVI1 : in     Vkm_GenIType) return Vkm_GenFType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Float input that returns a Vkm_Uint component-wise
    --< to a GenFType vector.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenFType vector returning 
    --< a GenUType vector.
    --<
    --<    RVU := [Func(IVF1.x) ... Func(IVF1.w)]
    --<
    --< @param IVF1
    --< The input GenFType parameter.
    --<
    --< @return
    --< The resulting GenUType vector, RVU.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISF : in     Vkm_Float) return Vkm_Uint;
    function Apply_Func_IVF_RVU (IVF1 : in     Vkm_GenFType) return Vkm_GenUType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Uint input that returns a Vkm_Float component-wise
    --< to a GenFType vector.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenUType vector returning 
    --< a GenFType vector.
    --<
    --<    RVF := [Func(IVU1.x) ... Func(IVU1.w)]
    --<
    --< @param IVU1
    --< The input GenUType parameter.
    --<
    --< @return
    --< The resulting GenFType vector, RVF.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISU : in     Vkm_Uint) return Vkm_Float;
    function Apply_Func_IVU_RVF (IVU1 : in     Vkm_GenUType) return Vkm_GenFType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Float input and a Vkm_Int outut that returns a Vkm_Fouble
    --< component-wise to the corresponding vector types.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenFType and GenIType
    --< vector, returning a GenFType vector.
    --<
    --<    RVF := [Func(IVF.x,OVI.x) ... Func(IVF.w,OVI.w)]
    --<
    --< @param IVF
    --< The input GenFType parameter.
    --<
    --< @param OVI
    --< The output GenIType parameter.
    --<
    --< @return
    --< The resulting GenFType vector, RVF.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISF : in     Vkm_Float;
                            OSI :    out Vkm_Int) return Vkm_Float;
    function Apply_Func_IVF_OVI_RVF(IVF : in     Vkm_GenFType;
                                    OVI :    out Vkm_GenIType) return Vkm_GenFType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on a Vkm_Float and a Vkm_Int input that returns a Vkm_Float 
    --< component-wise to the corresponding vector types.
    --<
    --< @description
    --< Applies a supplied function component wise on a GenFType and GenIType
    --< vector, returning a GenFType vector.
    --<
    --<    RVF := [Func(IVF.x,IVI.x) ... Func(IVF.w,IVI.w)]
    --<
    --< @param IVF
    --< The input GenFType parameter.
    --<
    --< @param IVI
    --< The input GenIType parameter.
    --<
    --< @return
    --< The resulting GenFType vector, RVF.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISF : in     Vkm_Float;
                            ISI : in     Vkm_Int) return Vkm_Float;
    function Apply_Func_IVF_IVI_RVF(IVF : in     Vkm_GenFType;
                                    IVI : in     Vkm_GenIType) return Vkm_GenFType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on two Vkm_Float inputs that returns a Vkm_Bool component-wise
    --< to two Vkm_GenFType vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenFType vectors, 
    --< returning a GenBType vector.
    --<
    --<    RVB := [Func(IVF1.x,IVF2.x) ... Func(IVF1.w,IVF2.w)]
    --<
    --< @param IVF1
    --< The first input GenFType parameter.
    --<
    --< @param IVF2
    --< The second input GenFType parameter.
    --<
    --< @return
    --< The resulting GenBType vector, RVB.
    ----------------------------------------------------------------------------
    generic
        with function Func (ISF1 : in     Vkm_Float;
                            ISF2 : in     Vkm_Float) return Vkm_Bool;
    function Apply_Func_IVF_IVF_RVB(IVF1, IVF2 : in     Vkm_GenFType) return Vkm_GenBType;

end Vulkan.Math.GenFType;
