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
use Vulkan.Math.GenBType;

package Vulkan.Math.GenFType is
    pragma Preelaborate;
    pragma Pure;

    package GFT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Float);

    subtype Vkm_GenFType is GFT.Vkm_GenType;


    ----------------------------------------------------------------------------
    -- Operations for Vkm_GenFType
    ----------------------------------------------------------------------------
    -- Subtraction Operators
    function "-" is new GFT.Apply_Func_IV_IV_RV("-");
    function "-" is new GFT.Apply_Func_IV_IS_RV("-");
    function "-" is new GFT.Apply_Func_IS_IV_RV("-");

    -- Multiplication Operators
    function "*" is new GFT.Apply_Func_IV_IV_RV("*");
    function "*" is new GFT.Apply_Func_IS_IV_RV("*");
    function "*" is new GFT.Apply_Func_IV_IS_RV("*");

    ----------------------------------------------------------------------------
    -- Generic Operations
    ----------------------------------------------------------------------------
    generic
        with function Func(ISF1, ISF2 : in     Vkm_Float;
                           ISB1       : in     Vkm_Bool ) return Vkm_Float;

    function Apply_Func_IVF_IVF_IVB_RVF(IVF1, IVF2 : in     Vkm_GenFType;
                                        IVB1       : in     Vkm_GenBType) return Vkm_GenFType;

end Vulkan.Math.GenFType;
