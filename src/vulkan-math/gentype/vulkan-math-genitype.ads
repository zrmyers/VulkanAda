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

use Vulkan.Math.GenBType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenType
--------------------------------------------------------------------------------
--< @summary
--< This package describes any length vector of Vkm_Int type.
--<
--< @description
--< Provides an instantiation of the generic GenType  package with a Base_Type of 
--< Vkm_Int. This is used to provide the Vkm_GenIType subtype for the Vulkan Math 
--< library.
--------------------------------------------------------------------------------
package Vulkan.Math.GenIType is
    pragma Preelaborate;
    pragma Pure;

    --< @private
    --< An instance of the generic GenType package, with Vkm_Int as the Base_Type.
    package GIT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Int, Default_Value => 0, Image => Vkm_Int'Image);

    --< A subtype of the instantiated Vkm_GenType that represents the GenIType 
    --< described in the GLSL specification.
    subtype Vkm_GenIType is GIT.Vkm_GenType;

    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function for parameters of Vkm_Int and Vkm_Bool type to GenIType
    --< and GenBType vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenIType vectors and
    --< a GenBType vector returning a GenIType vector.
    --<
    --<    RVI := [Func(IVI1.x, IVI2.x, IVB1.x) ... Func(IVI1.w, IVI2.w, IVB1.w)]
    --<
    --< @param IVI1
    --< The first input GenIType parameter.
    --<
    --< @param IVI2
    --< The second input GenIType parameter.
    --<
    --< @param IVB1
    --< The first input GenBType parameter.
    --<
    --< @return
    --< The result GenIType vector, RVI.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISI1, ISI2 : in     Vkm_Int;
                           ISB1       : in     Vkm_Bool ) return Vkm_Int;

    function Apply_Func_IVI_IVI_IVB_RVI(IVI1, IVI2 : in     Vkm_GenIType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenIType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function on two Vkm_Int inputs that returns a Vkm_Bool component-wise
    --< to two Vkm_GenIType vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenIType vectors, 
    --< returning a GenBType vector.
    --<
    --<    RVB := [Func(IVI1.x,IVI2.x) ... Func(IVI1.w,IVI2.w)]
    --<
    --< @param IVI1
    --< The first input GenIType parameter.
    --<
    --< @param IVI2
    --< The second input GenIType parameter.
    --<
    --< @return
    --< The resulting GenBType vector, RVB.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISI1, ISI2 : in     Vkm_Int) return Vkm_Bool;
    function Apply_Func_IVI_IVI_RVB(IVI1, IVI2 : in     Vkm_GenIType) return Vkm_GenBType;

end Vulkan.Math.GenIType;
