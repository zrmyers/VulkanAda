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
with Vulkan.Math.GenBType;
with Vulkan.Math.Bvec3;
with Vulkan.Math.Bvec2;

use Vulkan.Math.GenBType;
use Vulkan.Math.Bvec3;
use Vulkan.Math.Bvec2;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package defines a boolean vector type with 4 components.
--------------------------------------------------------------------------------
package Vulkan.Math.Bvec4 is
    pragma Preelaborate;
    pragma Pure;

    --< A 4-compoent vector of boolean values.
    subtype Vkm_Bvec4 is Vkm_GenBType(Last_Index => 3);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a Bvec4 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Bvec4:
    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a default vector with all components set to false.
    --<
    --< @return
    --< A 4D boolean vector with all components set to false.
    ----------------------------------------------------------------------------
    function Make_Bvec4 return Vkm_Bvec4 is
        (GBT.Make_GenType(Last_Index => 3, value => false)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a vector with all components set to the same value.
    --<
    --< @param scalar_value
    --< The value to set all components to.
    --<
    --< @return
    --< A 4D boolean vector with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Bvec4 (scalar_value : in     Vkm_Bool) return Vkm_Bvec4 is
        (GBT.Make_GenType(Last_Index => 3, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a vector by copying components from an existing vector.
    --<
    --< @param vec4_value
    --< The Bvec4 to copy components from.
    --<
    --< @return
    --< A 4D boolean vector with all of its components set equal to the corresponding
    --< components of vec4_value.
    ----------------------------------------------------------------------------
    function Make_Bvec4 (vec4_value : in     Vkm_Bvec4) return Vkm_Bvec4 is
        (GBT.Make_GenType(vec4_value.data(0),vec4_value.data(1),
                          vec4_value.data(2),vec4_value.data(3))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
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
    --< @param value4
    --< Value for component 4.
    --<
    --< @return
    --< A 4D boolean vector with all components set as specified.
    ----------------------------------------------------------------------------
    function Make_Bvec4 (value1, value2, value3, value4 : in    Vkm_Bool) return Vkm_Bvec4
        renames GBT.Make_GenType;

    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a vector by concatenating a scalar value with a Bvec3.
    --<
    --<     Bvec4 = [scalar_value, vec3_value  ]
    --<
    --< @param scalar_value
    --< The scalar value to concatenate with the Bvec3.
    --<
    --< @param vec3_value
    --< The Bvec3 to concatenate to the scalar value.
    --<
    --< @return The instance of Bvec4.
    ----------------------------------------------------------------------------
    function Make_Bvec4 (scalar_value : in     Vkm_Bool;
                         vec3_value   : in     Vkm_Bvec3) return Vkm_Bvec3 is
        (Make_Bvec4(scalar_value,
                    vec3_value.data(0),
                    vec3_value.data(1),
                    vec3_value.data(2))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a vector by concatenating a Bvec3 with a scalar value.
    --<
    --<     Bvec4 = [vec3_value, scalar_value]
    --<
    --< @param vec3_value
    --< The Bvec3 to concatenate to the scalar value.
    --<
    --< @param scalar_value
    --< The scalar value to concatenate to the Bvec3.
    --<
    --< @return
    --< The instance of Bvec4.
    ----------------------------------------------------------------------------
    function Make_Bvec4 (vec3_value   : in     Vkm_Bvec3;
                         scalar_value : in     Vkm_Bool) return Vkm_Bvec4 is
        (Make_Bvec4(vec3_value.data(0),
                    vec3_value.data(1),
                    vec3_value.data(2),
                    scalar_value      )) with Inline;


    ---------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a vector by concatenating a Bvec2 with a Bvec2.
    --<
    --<     Bvec4 = [vec2_value1, vec2_value2]
    --<
    --< @param vec2_value1
    --< The first Bvec2.
    --<
    --< @param vec2_value2
    --< The second Bvec2.
    --<
    --< @return
    --< The instance of Bvec4.
    ---------------------------------------------------------------------------
    function Make_Bvec4 (vec2_value1, vec2_value2 : in     Vkm_Bvec2) return Vkm_Bvec4 is
        (Make_Bvec4(vec2_value1.data(0),vec2_value1.data(1),
                    vec2_value2.data(0),vec2_value2.data(1)));


    ---------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a vector by concatenating two scalar values with a Bvec2.
    --<
    --<     Bvec4 = [scalar1, scalar2, vec2_value]
    --<
    --< @param scalar1
    --< First scalar value.
    --<
    --< @param scalar2
    --< Second scalar value.
    --<
    --< @param vec2_value
    --< The Bvec2 value.
    --<
    --< @return
    --< The instance of Bvec4.
    ---------------------------------------------------------------------------
    function Make_Bvec4 (scalar1, scalar2 : in     Vkm_Bool;
                         vec2_value       : in     Vkm_Bvec2) return Vkm_Bvec4 is
        (Make_Bvec4(scalar1, scalar2, vec2_value.data(0),vec2_value.data(1)));


    ---------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a vector by concatenating two scalar values with a Bvec2.
    --<
    --<     Bvec4 = [scalar1, vec2_value, scalar2]
    --<
    --< @param scalar1
    --< First scalar value.
    --<
    --< @param vec2_value
    --< The Bvec2 value.
    --<
    --< @param scalar2
    --< Second scalar value.
    --<
    --< @return
    --< The instance of Bvec4.
    ---------------------------------------------------------------------------
    function Make_Bvec4 (scalar1     : in     Vkm_Bool;
                         vec2_value : in     Vkm_Bvec2 ;
                         scalar2     : in     Vkm_Bool) return Vkm_Bvec4 is
        (Make_Bvec4(scalar1, vec2_value.data(0),vec2_value.data(1), scalar2));


    ---------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Bvec4 type.
    --<
    --< @description
    --< Produce a vector by concatenating two scalar values with a Bvec2.
    --<
    --< Bvec4 = [vec2_value, scalar1, scalar2]
    --<
    --< @param vec2_value
    --< The Bvec2 value.
    --<
    --< @param scalar1
    --< First scalar value.
    --<
    --< @param scalar2
    --< Second scalar value.
    --<
    --< @return
    --< The instance of Bvec4.
    ---------------------------------------------------------------------------
    function Make_Bvec4 (vec2_value : in     Vkm_Bvec2 ;
                         scalar1     : in     Vkm_Bool;
                         scalar2     : in     Vkm_Bool) return Vkm_Bvec4 is
        (Make_Bvec4(vec2_value.data(0),vec2_value.data(1), scalar1, scalar2));


end Vulkan.Math.Bvec4;
