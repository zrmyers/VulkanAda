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
--
-- This package provides operator definitions for all types.
--------------------------------------------------------------------------------
with Vulkan.Math.GenFType;
with Vulkan.Math.GenDType;
with Vulkan.Math.GenBType;
with Vulkan.Math.GenIType;
with Vulkan.Math.GenUType;

use Vulkan.Math.GenFType;
use Vulkan.Math.GenDType;
use Vulkan.Math.GenIType;
use Vulkan.Math.GenUType;
use Vulkan.Math.GenBType;

package Vulkan.Math.Relational is
    pragma Preelaborate;
    pragma Pure;


    ----------------------------------------------------------------------------
    -- @brief
    -- Component-wise compare of x < y
    --
    -- @param[in]     x The left parameter.
    -- @param[in]     y The right parameter.
    --
    -- @return x < y
    ----------------------------------------------------------------------------
    function Less_Than is new Apply_Func_IVF_IVF_RVB("<");
    function Less_Than is new Apply_Func_IVD_IVD_RVB("<");
    function Less_Than is new Apply_Func_IVI_IVI_RVB("<");
    function Less_Than is new Apply_Func_IVU_IVU_RVB("<");


    ----------------------------------------------------------------------------
    -- @brief
    -- Component-wise compare of x <= y
    --
    -- @param[in]     x The left parameter.
    -- @param[in]     y The right parameter.
    --
    -- @return x <= y
    ----------------------------------------------------------------------------
    function Less_Than_Equal is new Apply_Func_IVF_IVF_RVB("<=");
    function Less_Than_Equal is new Apply_Func_IVD_IVD_RVB("<=");
    function Less_Than_Equal is new Apply_Func_IVI_IVI_RVB("<=");
    function Less_Than_Equal is new Apply_Func_IVU_IVU_RVB("<=");


    ----------------------------------------------------------------------------
    -- @brief
    -- Component-wise compare of x > y
    --
    -- @param[in]     x The left parameter.
    -- @param[in]     y The right parameter.
    --
    -- @return x > y
    ----------------------------------------------------------------------------
    function Greater_Than is new Apply_Func_IVF_IVF_RVB(">");
    function Greater_Than is new Apply_Func_IVD_IVD_RVB(">");
    function Greater_Than is new Apply_Func_IVI_IVI_RVB(">");
    function Greater_Than is new Apply_Func_IVU_IVU_RVB(">");


    ----------------------------------------------------------------------------
    -- @brief
    -- Component-wise compare of x >= y
    --
    -- @param[in]     x The left parameter.
    -- @param[in]     y The right parameter.
    --
    -- @return x >= y
    ----------------------------------------------------------------------------
    function Greater_Than_Equal is new Apply_Func_IVF_IVF_RVB(">=");
    function Greater_Than_Equal is new Apply_Func_IVD_IVD_RVB(">=");
    function Greater_Than_Equal is new Apply_Func_IVI_IVI_RVB(">=");
    function Greater_Than_Equal is new Apply_Func_IVU_IVU_RVB(">=");


    ----------------------------------------------------------------------------
    -- @brief
    -- Component-wise compare of x == y
    --
    -- @param[in]     x The left parameter.
    -- @param[in]     y The right parameter.
    --
    -- @return x == y
    ----------------------------------------------------------------------------
    function Equal is new Apply_Func_IVF_IVF_RVB ("=");
    function Equal is new Apply_Func_IVD_IVD_RVB ("=");
    function Equal is new Apply_Func_IVI_IVI_RVB ("=");
    function Equal is new Apply_Func_IVU_IVU_RVB ("=");
    function Equal is new GBT.Apply_Func_IV_IV_RV("=");

    ----------------------------------------------------------------------------
    -- @brief
    -- Component-wise compare of x != y
    --
    -- @param[in]     x The left parameter.
    -- @param[in]     y The right parameter.
    --
    -- @return x != y
    ----------------------------------------------------------------------------
    function Not_Equal is new Apply_Func_IVF_IVF_RVB ("/=");
    function Not_Equal is new Apply_Func_IVD_IVD_RVB ("/=");
    function Not_Equal is new Apply_Func_IVI_IVI_RVB ("/=");
    function Not_Equal is new Apply_Func_IVU_IVU_RVB ("/=");
    function Not_Equal is new GBT.Apply_Func_IV_IV_RV("/=");


    ----------------------------------------------------------------------------
    -- @brief
    -- Returns true if any component of x is true.
    --
    -- @param[in]     x The parameter 'x'.
    --
    -- @return True if any component of x is true. Otherwise false.
    ----------------------------------------------------------------------------
    function Is_Any (x : in     Vkm_GenBType) return Vkm_Bool;


    ----------------------------------------------------------------------------
    -- @brief
    -- Returns true if all of the components of x are true.
    --
    -- @param[in]     x The parameter 'x'.
    --
    -- @return True if all components of 'x' are true. Otherwise false.
    ----------------------------------------------------------------------------
    function Is_All (x : in     Vkm_GenBType) return Vkm_Bool;


    ----------------------------------------------------------------------------
    -- @brief
    -- Returns the component-wise complement of each component of x.
    --
    -- @param[in]     x The parameter 'x'
    --
    -- @return The component-wise complement of x.
    ----------------------------------------------------------------------------
    function Complement is new GBT.Apply_Func_IV_RV("not");


end Vulkan.Math.Relational;
