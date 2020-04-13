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
--< This package describes any length vector of Vkm_Uint type.
--<
--< @description
--< Provides an instatiation of the generic GenType  package with a Base_Type of 
--< Vkm_Uint. This is used to provide the GenUType subtype for the Vulkan Math 
--< library.
--------------------------------------------------------------------------------
package Vulkan.Math.GenUType is
    pragma Preelaborate;
    pragma Pure;

    ----------------------------------------------------------------------------
    --< @private
    --< An instance of the generic GenType package, with Vkm_Uint as the Base_Type.
    package GUT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Uint, Image => Vkm_Uint'Image);

    --< A subtype of the instantiated Vkm_GenType that represents the GenUType 
    --< described in the GLSL specification.
    subtype Vkm_GenUType is GUT.Vkm_GenType;

    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function for parameters of Vkm_Uint and Vkm_Bool type to vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenUType vectors and
    --< a GenBType vector returning a GenUType vector.
    --<
    --<    RVU := [Func(IVU1.x, IVU2.x, IVB1.x) ... Func(IVU1.w, IVU2.w, IVB1.w)]
    --<
    --< @param IVU1
    --< The first input GenUType parameter.
    --<
    --< @param IVU2
    --< The second input GenUType parameter.
    --<
    --< @param IVB1
    --< The first input GenBType parameter.
    --<
    --< @return
    --< The result GenUType vector, RVU.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISF1, ISF2 : in     Vkm_Uint;
                           ISB1       : in     Vkm_Bool ) return Vkm_Uint;

    function Apply_Func_IVU_IVU_IVB_RVU(IVU1, IVU2 : in     Vkm_GenUType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenUType;


    ---------------------------------------------------------------------------- 
    --< @summary
    --< Apply function for parameters of Vkm_Uint and Vkm_Bool type to vectors.
    --<
    --< @description
    --< Applies a supplied function component wise on two GenUType vectors and
    --< returns a GenBType vector.
    --<
    --<    RVB := [Func(IVU1.x, IVU2.x) ... Func(IVU1.w, IVU2.w)]
    --<
    --< @param IVU1
    --< The first input GenUType parameter.
    --<
    --< @param IVU2
    --< The second input GenUType parameter.
    --<
    --< @return
    --< The result GenBType vector, RVB.
    ----------------------------------------------------------------------------
    generic
        with function Func(ISU1, ISU2 : in     Vkm_Uint) return Vkm_Bool;
    function Apply_Func_IVU_IVU_RVB(IVU1, IVU2 : in     Vkm_GenUType) return Vkm_GenBType;


end Vulkan.Math.GenUType;
