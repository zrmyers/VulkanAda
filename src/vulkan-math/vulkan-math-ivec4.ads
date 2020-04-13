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
with Vulkan.Math.Ivec3;
with Vulkan.Math.Ivec2;

use Vulkan.Math.GenIType;
use Vulkan.Math.Ivec3;
use Vulkan.Math.Ivec2;

--------------------------------------------------------------------------------
--< @group Vulkan Math Interface
--------------------------------------------------------------------------------
--< @summary
--< This package defines a 32-bit signed integer vector type with 4 components.
--------------------------------------------------------------------------------
package Vulkan.Math.Ivec4 is
    pragma Preelaborate;
    pragma Pure;

    --< A 4-compoent floating point vector.
    subtype Vkm_Ivec4 is Vkm_GenIType(Last_Index => 3);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a Ivec4 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Ivec4:
    ----------------------------------------------------------------------------
    --< @description
    --< Produce a default vector with all components set to 0.0.
    --
    --< @return a Ivec4 with all components set to 0.0.
    ----------------------------------------------------------------------------
    function Make_Ivec4 return Vkm_Ivec4 is
        (GIT.Make_GenType(Last_Index => 3, value => 0)) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector with all components set to the same value.
    --
    --< @param     scalar_value The value to set all components to.
    --
    --< @return A Ivec4 with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Ivec4 (scalar_value : in     Vkm_Int) return Vkm_Ivec4 is
        (GIT.Make_GenType(Last_Index => 3, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector by copying components from an existing vector.
    --
    --< @param     vec4_value The Ivec4 to copy components from.
    --
    --< @return A Ivec4 with all of its components set equal to the corresponding
    --<          components of vec4_value.
    ----------------------------------------------------------------------------
    function Make_Ivec4 (vec4_value : in     Vkm_Ivec4) return Vkm_Ivec4 is
        (GIT.Make_GenType(vec4_value.data(0),vec4_value.data(1),
                  vec4_value.data(2),vec4_value.data(3))) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector by specifying the values for each of its components.
    --
    --< @param     value1 Value for component 1.
    --< @param     value2 Value for component 2.
    --< @param     value3 Value for componetn 3.
    --< @param     value4 value for component 4.
    --
    --< @return A Ivec4 with all components set as specified.
    ----------------------------------------------------------------------------
    function Make_Ivec4 (value1, value2, value3, value4 : in    Vkm_Int) return Vkm_Ivec4
        renames GIT.Make_GenType;

    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector by concatenating a scalar value with a Ivec3.
    --
    --< Ivec4 = [scalar_value, vec3_value  ]
    --
    --< @param     scalar_value The scalar value to concatenate with the Ivec3.
    --< @param     vec3_value   The Ivec3 to concatenate to the scalar value.
    --
    --< @return The instance of Ivec4.
    ----------------------------------------------------------------------------
    function Make_Ivec4 (scalar_value : in     Vkm_Int;
                         vec3_value   : in     Vkm_Ivec3) return Vkm_Ivec3 is
        (Make_Ivec4(scalar_value,
                    vec3_value.data(0),
                    vec3_value.data(1),
                    vec3_value.data(2))) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector by concatenating a Ivec3 with a scalar value.
    --
    --< Ivec4 = [vec3_value, scalar_value]
    --
    --< @param     vec3_value   The Ivec3 to concatenate to the scalar value.
    --< @param     scalar_value The scalar value to concatenate to the Ivec3.
    --
    --< @return The instance of Ivec4.
    ----------------------------------------------------------------------------
    function Make_Ivec4 (vec3_value   : in     Vkm_Ivec3;
                         scalar_value : in     Vkm_Int) return Vkm_Ivec4 is
        (Make_Ivec4(vec3_value.data(0),
                    vec3_value.data(1),
                    vec3_value.data(2),
                    scalar_value      )) with Inline;


     ---------------------------------------------------------------------------
     --< @description
     --< Produce a vector by concatenating a Ivec2 with a Ivec2.
     --
     --< Ivec4 = [vec2_value1, vec2_value2]
     --
     --< @param     vec2_value1 The first Ivec2.
     --< @param     vec2_value2 The second Ivec2.
     --
     --< @return The instance of Ivec4.
     ---------------------------------------------------------------------------
     function Make_Ivec4 (vec2_value1, vec2_value2 : in     Vkm_Ivec2) return Vkm_Ivec4 is
         (Make_Ivec4(vec2_value1.data(0),vec2_value1.data(1),
                     vec2_value2.data(0),vec2_value2.data(1)));


     ---------------------------------------------------------------------------
     --< @description
     --< Produce a vector by concatenating two scalar values with a Ivec2.
     --
     --< Ivec4 = [scalar1, scalar2, vec2_value]
     --
     --< @param     scalar1    First scalar value.
     --< @param     scalar2    Second scalar value.
     --< @param     vec2_value The Ivec2 value.
     --
     --< @return The instance of Ivec4.
     ---------------------------------------------------------------------------
     function Make_Ivec4 (scalar1, scalar2 : in     Vkm_Int;
                          vec2_value       : in     Vkm_Ivec2) return Vkm_Ivec4 is
         (Make_Ivec4(scalar1, scalar2, vec2_value.data(0),vec2_value.data(1)));


     ---------------------------------------------------------------------------
     --< @description
     --< Produce a vector by concatenating two scalar values with a Ivec2.
     --
     --< Ivec4 = [scalar1, vec2_value, scalar2]
     --
     --< @param     scalar1    First scalar value.
     --< @param     vec2_value The Ivec2 value.
     --< @param     scalar2    Second scalar value.
     --
     --< @return The instance of Ivec4.
     ---------------------------------------------------------------------------
     function Make_Ivec4 (scalar1    : in     Vkm_Int;
                          vec2_value : in     Vkm_Ivec2 ;
                          scalar2    : in     Vkm_Int) return Vkm_Ivec4 is
         (Make_Ivec4(scalar1, vec2_value.data(0),vec2_value.data(1), scalar2));


     ---------------------------------------------------------------------------
     --< @description
     --< Produce a vector by concatenating two scalar values with a Ivec2.
     --
     --< Ivec4 = [vec2_value, scalar1, scalar2]
     --
     --< @param     vec2_value The Ivec2 value.
     --< @param     scalar1    First scalar value.
     --< @param     scalar2    Second scalar value.
     --
     --< @return The instance of Ivec4.
     ---------------------------------------------------------------------------
     function Make_Ivec4 (vec2_value : in     Vkm_Ivec2 ;
                          scalar1     : in     Vkm_Int;
                          scalar2     : in     Vkm_Int) return Vkm_Ivec4 is
         (Make_Ivec4(vec2_value.data(0),vec2_value.data(1), scalar1, scalar2));


end Vulkan.Math.Ivec4;
