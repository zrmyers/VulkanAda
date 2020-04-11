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
with Vulkan.Math.GenUType;
with Vulkan.Math.Uvec3;
with Vulkan.Math.Uvec2;

use Vulkan.Math.GenUType;
use Vulkan.Math.Uvec3;
use Vulkan.Math.Uvec2;

package Vulkan.Math.Uvec4 is
    pragma Preelaborate;
    pragma Pure;

    -- A 4-compoent floating point vector.
    subtype Vkm_Uvec4 is Vkm_GenUType(Last_Index => 3);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a Uvec4 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Uvec4:
    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a default vector with all components set to 0.0.
    --
    -- @returns a Uvec4 with all components set to 0.0.
    ----------------------------------------------------------------------------
    function Make_Uvec4 return Vkm_Uvec4 is
        (GUT.Make(Last_Index => 3, value => 0)) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector with all components set to the same value.
    --
    -- @param[in]     scalar_value The value to set all components to.
    --
    -- @returns A Uvec4 with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Uvec4 (scalar_value : in     Vkm_Uint) return Vkm_Uvec4 is
        (GUT.Make(Last_Index => 3, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by copying components from an existing vector.
    --
    -- @param[in]     vec4_value The Uvec4 to copy components from.
    --
    -- @returns A Uvec4 with all of its components set equal to the corresponding
    --          components of vec4_value.
    ----------------------------------------------------------------------------
    function Make_Uvec4 (vec4_value : in     Vkm_Uvec4) return Vkm_Uvec4 is
        (GUT.Make(vec4_value.data(0),vec4_value.data(1),
                  vec4_value.data(2),vec4_value.data(3))) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by specifying the values for each of its components.
    --
    -- @param[in]     value1 Value for component 1.
    -- @param[in]     value2 Value for component 2.
    -- @param[in]     value3 Value for componetn 3.
    -- @param[in]     Value4 value for component 4.
    --
    -- @return A Uvec4 with all components set as specified.
    ----------------------------------------------------------------------------
    function Make_Uvec4 (value1, value2, value3, value4 : in    Vkm_Uint) return Vkm_Uvec4
        renames GUT.Make;

    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by concatenating a scalar value with a Uvec3.
    --
    -- Uvec4 = [scalar_value, vec3_value  ]
    --
    -- @param[in]     scalar_value The scalar value to concatenate with the Uvec3.
    -- @param[in]     vec3_value   The Uvec3 to concatenate to the scalar value.
    --
    -- @returns The instance of Uvec4.
    ----------------------------------------------------------------------------
    function Make_Uvec4 (scalar_value : in     Vkm_Uint;
                         vec3_value   : in     Vkm_Uvec3) return Vkm_Uvec3 is
        (Make_Uvec4(scalar_value,
                    vec3_value.data(0),
                    vec3_value.data(1),
                    vec3_value.data(2))) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by concatenating a Uvec3 with a scalar value.
    --
    -- Uvec4 = [vec3_value, scalar_value]
    --
    -- @param[in]     vec3_value   The Uvec3 to concatenate to the scalar value.
    -- @param[in]     scalar_value The scalar value to concatenate to the Uvec3.
    --
    -- @returns The instance of Uvec4.
    ----------------------------------------------------------------------------
    function Make_Uvec4 (vec3_value   : in     Vkm_Uvec3;
                         scalar_value : in     Vkm_Uint) return Vkm_Uvec4 is
        (Make_Uvec4(vec3_value.data(0),
                    vec3_value.data(1),
                    vec3_value.data(2),
                    scalar_value      )) with Inline;


     ---------------------------------------------------------------------------
     -- @brief
     -- Produce a vector by concatenating a Uvec2 with a Uvec2.
     --
     -- Uvec4 = [vec2_value1, vec2_value2]
     --
     -- @param[in]     vec2_value1 The first Uvec2.
     -- @param[in]     vec2_value2 The second Uvec2.
     --
     -- @returns The instance of Uvec4.
     ---------------------------------------------------------------------------
     function Make_Uvec4 (vec2_value1, vec2_value2 : in     Vkm_Uvec2) return Vkm_Uvec4 is
         (Make_Uvec4(vec2_value1.data(0),vec2_value1.data(1),
                     vec2_value2.data(0),vec2_value2.data(1)));


     ---------------------------------------------------------------------------
     -- @brief
     -- Produce a vector by concatenating two scalar values with a Uvec2.
     --
     -- Uvec4 = [scalar1, scalar2, vec2_value]
     --
     -- @param[in]     scalar1    First scalar value.
     -- @param[in]     scalar2    Second scalar value.
     -- @param[in]     vec2_value The Uvec2 value.
     --
     -- @returns The instance of Uvec4.
     ---------------------------------------------------------------------------
     function Make_Uvec4 (scalar1, scalar2 : in     Vkm_Uint;
                          vec2_value       : in     Vkm_Uvec2) return Vkm_Uvec4 is
         (Make_Uvec4(scalar1, scalar2, vec2_value.data(0),vec2_value.data(1)));


     ---------------------------------------------------------------------------
     -- @brief
     -- Produce a vector by concatenating two scalar values with a Uvec2.
     --
     -- Uvec4 = [scalar1, vec2_value, scalar2]
     --
     -- @param[in]     scalar1    First scalar value.
     -- @param[in]     vec2_value The Uvec2 value.
     -- @param[in]     scalar2    Second scalar value.
     --
     -- @returns The instance of Uvec4.
     ---------------------------------------------------------------------------
     function Make_Uvec4 (scalar1     : in     Vkm_Uint;
                          vec2_value : in     Vkm_Uvec2 ;
                          scalar2     : in     Vkm_Uint) return Vkm_Uvec4 is
         (Make_Uvec4(scalar1, vec2_value.data(0),vec2_value.data(1), scalar2));


     ---------------------------------------------------------------------------
     -- @brief
     -- Produce a vector by concatenating two scalar values with a Uvec2.
     --
     -- Uvec4 = [vec2_value, scalar1, scalar2]
     --
     -- @param[in]     vec2_value The Uvec2 value.
     -- @param[in]     scalar1    First scalar value.
     -- @param[in]     scalar2    Second scalar value.
     --
     -- @returns The instance of Uvec4.
     ---------------------------------------------------------------------------
     function Make_Uvec4 (vec2_value : in     Vkm_Uvec2 ;
                          scalar1     : in     Vkm_Uint;
                          scalar2     : in     Vkm_Uint) return Vkm_Uvec4 is
         (Make_Uvec4(vec2_value.data(0),vec2_value.data(1), scalar1, scalar2));


end Vulkan.Math.Uvec4;
