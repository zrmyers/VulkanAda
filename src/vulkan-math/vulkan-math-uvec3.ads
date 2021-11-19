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
-- This package describes a Boolean Vector with 3 components.
--------------------------------------------------------------------------------
with Vulkan.Math.GenUType;
with Vulkan.Math.Uvec2;

use Vulkan.Math.GenUType;
use Vulkan.Math.Uvec2;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides a 32-bit unsigned integer vector type with 3 components.
--------------------------------------------------------------------------------
package Vulkan.Math.Uvec3 is
    pragma Preelaborate;
    pragma Pure;

    subtype Vkm_Uvec3 is Vkm_GenUType(Last_Index => 2);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a dvec3 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Uvec3:
    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Uvec3 type.
    --<
    --< @description
    --< Produce a default vector with all components set to 0.
    --<
    --< @return 
    --< A Uvec3 with all components set to 0.
    ----------------------------------------------------------------------------
    function Make_Uvec3 return Vkm_Uvec3 is
        (GUT.Make_GenType(Last_Index => 2, value => 0)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Uvec3 type.
    --<
    --< @description
    --< Produce a vector with all components set to the same value.
    --<
    --< @param scalar_value
    --< The value to set all components to.
    --<
    --< @return 
    --< A Uvec3 with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Uvec3 (scalar_value : in     Vkm_Uint) return Vkm_Uvec3 is
        (GUT.Make_GenType(Last_Index => 2, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Uvec3 type.
    --<
    --< @description
    --< Produce a vector by copying components from an existing vector.
    --<
    --< @param vec3_value 
    --< The Uvec3 to copy components from.
    --<
    --< @return 
    --< A Uvec3 with all of its components set equal to the corresponding
    --< components of vec3_value.
    ----------------------------------------------------------------------------
    function Make_Uvec3 (vec3_value : in     Vkm_Uvec3) return Vkm_Uvec3 is
        (GUT.Make_GenType(vec3_value.data(0),vec3_value.data(1), vec3_value.data(2))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Uvec3 type.
    --<
    --< @description
    --< Produce a vector by specifying the values for each of its components.
    --<
    --< @param value1
    --< Value for component 1.
    --<
    --< @param value2
    --< Value for component 2.
    --<
    --< @param value3
    --< Value for component 3.
    --<
    --< @return A Uvec3 with all components set as specified.
    ----------------------------------------------------------------------------
    function Make_Uvec3 (value1, value2, value3 : in    Vkm_Uint) return Vkm_Uvec3
        renames GUT.Make_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Uvec3 type.
    --<
    --< @description
    --< Produce a vector by concatenating a scalar float with a vec2.
    --<
    --<     Uvec3 = [scalar_value, vec2_value]
    --<
    --< @param scalar_value
    --< The scalar value to concatenate with the Uvec3.
    --<
    --< @param vec2_value
    --< The vec2 to concatenate to the scalar value.
    --<
    --< @return 
    --< The instance of Uvec3.
    ----------------------------------------------------------------------------
    function Make_Uvec3 (scalar_value : in     Vkm_Uint;
                         vec2_value   : in     Vkm_Uvec2 ) return Vkm_Uvec3 is
        (Make_Uvec3(scalar_value, vec2_value.x, vec2_value.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Uvec3 type.
    --<
    --< @description
    --< Produce a vector by concatenating a scalar float with a vec2.
    --<
    --<     Uvec3 = [vec2_value, scalar_value]
    --<
    --< @param vec2_value
    --< The vec2 to concatenate to the scalar value.
    --<
    --< @param scalar_value
    --< The scalar value to concatenate with the Uvec3.
    --<
    --< @return 
    --< The instance of Uvec3.
    ----------------------------------------------------------------------------
    function Make_Uvec3 (vec2_value   : in     Vkm_Uvec2;
                         scalar_value : in     Vkm_Uint ) return Vkm_Uvec3 is
        (Make_Uvec3(vec2_value.x, vec2_value.y, scalar_value)) with Inline;


end Vulkan.Math.Uvec3;
