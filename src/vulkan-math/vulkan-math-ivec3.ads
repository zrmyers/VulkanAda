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
with Vulkan.Math.GenIType;
with Vulkan.Math.Ivec2;

use Vulkan.Math.GenIType;
use Vulkan.Math.Ivec2;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package defines a 32-bit signed integer vector type with 3 components.
--------------------------------------------------------------------------------
package Vulkan.Math.Ivec3 is
    pragma Preelaborate;
    pragma Pure;

    --< A 3 component vector of 32-bit signed integers.
    subtype Vkm_Ivec3 is Vkm_GenIType(Last_Index => 2);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a dvec3 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Ivec3:
    ----------------------------------------------------------------------------
    --< @description
    --< Produce a default vector with all components set to 0.
    --
    --< @return a Ivec3 with all components set to 0.0.
    ----------------------------------------------------------------------------
    function Make_Ivec3 return Vkm_Ivec3 is
        (GIT.Make_GenType(Last_Index => 2, value => 0)) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector with all components set to the same value.
    --
    --< @param     scalar_value The value to set all components to.
    --
    --< @return An Ivec3 with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Ivec3 (scalar_value : in     Vkm_Int) return Vkm_Ivec3 is
        (GIT.Make_GenType(Last_Index => 2, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector by copying components from an existing vector.
    --
    --< @param vec3_value 
    --< The Ivec3 to copy components from.
    --
    --< @returns 
    --< An Ivec3 with all of its components set equal to the corresponding
    --< components of vec3_value.
    ----------------------------------------------------------------------------
    function Make_Ivec3 (vec3_value : in     Vkm_Ivec3) return Vkm_Ivec3 is
        (GIT.Make_GenType(vec3_value.data(0),vec3_value.data(1), vec3_value.data(2))) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector by specifying the values for each of its components.
    --
    --< @param     value1 Value for component 1.
    --< @param     value2 Value for component 2.
    --< @param     value3 Value for componetn 3.
    --
    --< @return An Ivec3 with all components set as specified.
    ----------------------------------------------------------------------------
    function Make_Ivec3 (value1, value2, value3 : in    Vkm_Int) return Vkm_Ivec3
        renames GIT.Make_GenType;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector by concatenating a scalar float with a vec2.
    --
    --< Ivec3 = [scalar_value, vec2_value]
    --
    --< @param     scalar_value The scalar value to concatenate with the Ivec3.
    --< @param     vec2_value   The vec2 to concatenate to the scalar value.
    --
    --< @returns The instance of Ivec3.
    ----------------------------------------------------------------------------
    function Make_Ivec3 (scalar_value : in     Vkm_Int;
                         vec2_value   : in     Vkm_Ivec2 ) return Vkm_Ivec3 is
        (Make_Ivec3(scalar_value, vec2_value.x, vec2_value.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector by concatenating a scalar float with a vec2.
    --
    --< Ivec3 = [vec2_value, scalar_value]
    --
    --< @param     vec2_value   The vec2 to concatenate to the scalar value.
    --< @param     scalar_value The scalar value to concatenate with the Ivec3.
    --
    --< @returns The instance of Ivec3.
    ----------------------------------------------------------------------------
    function Make_Ivec3 (vec2_value   : in     Vkm_Ivec2;
                         scalar_value : in     Vkm_Int ) return Vkm_Ivec3 is
        (Make_Ivec3(vec2_value.x, vec2_value.y, scalar_value)) with Inline;



end Vulkan.Math.Ivec3;
