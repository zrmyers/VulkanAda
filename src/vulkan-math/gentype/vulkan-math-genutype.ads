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
with Vulkan.Math.GenType;
with Vulkan.Math.GenBType;
use Vulkan.Math.GenBType;

package Vulkan.Math.GenUType is
    pragma Preelaborate;
    pragma Pure;

    package GUT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Uint);

    subtype Vkm_GenUType is GUT.Vkm_GenType;

    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    generic
        with function Func(ISF1, ISF2 : in     Vkm_Uint;
                           ISB1       : in     Vkm_Bool ) return Vkm_Uint;

    function Apply_Func_IVU_IVU_IVB_RVU(IVU1, IVU2 : in     Vkm_GenUType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenUType;

end Vulkan.Math.GenUType;
