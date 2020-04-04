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
-- This package describes a Floating Point Vector with 4 components.
--------------------------------------------------------------------------------
with Vulkan.Math.GenFType;
use Vulkan.Math.GenFType;

package Vulkan.Math.Vec2 is


    subtype Vkm_Vec2 is Vkm_GenFType(Last_Index => 1);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a vec2 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Vec4:
    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a default vector with all components set to 0.0.
    --
    -- @returns a Vec2 with all components set to 0.0.
    ----------------------------------------------------------------------------
    function Make_Vec3 return Vkm_Vec2 is
        (GFT.Make(Last_Index => 2, value => 0.0)) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector with all components set to the same value.
    --
    -- @param[in]     scalar_value The value to set all components to.
    --
    -- @returns A Vec4 with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Vec3 (scalar_value : in     Vkm_Float) return Vkm_Vec2 is
        (GFT.Make(Last_Index => 2, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by concatenating a scalar float with a vec2.
    --
    -- vec3 = [scalar_value, vec2_value]
    --
    -- @param[in]     scalar_value The scalar value to concatenate with the vec3.
    -- @param[in]     vec2_value   The vec2 to concatenate to the scalar value.
    --
    -- @returns The instance of Vec3.
    ----------------------------------------------------------------------------

end Vulkan.Math.Vec2;
