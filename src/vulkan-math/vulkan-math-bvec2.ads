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
with Vulkan.Math.GenBType;
use Vulkan.Math.GenBType;

--------------------------------------------------------------------------------
--< @group Vulkan Math Interface
--------------------------------------------------------------------------------
--< @summary
--< This package defines a boolean vector type with 2 components.
--------------------------------------------------------------------------------
package Vulkan.Math.Bvec2 is
    pragma Preelaborate;
    pragma Pure;

    subtype Vkm_Bvec2 is Vkm_GenBType(Last_Index => 1);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a vec2 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Vec2:
    ----------------------------------------------------------------------------
    --> @description
    --> Produce a default vector with all components set to False.
    -->
    --> @return a Vec2 with all components set to false.
    ----------------------------------------------------------------------------
    function Make_Bvec2 return Vkm_Bvec2 is
        (GBT.Make_GenType(Last_Index => 1, value => False)) with Inline;


    ----------------------------------------------------------------------------
    --< @description
    --< Produce a vector with all components set to the same value.
    --<
    --< @param scalar_value
    --< The value to set all components to.
    --<
    --< @return 
    --< A 2D vector with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Bvec2 (scalar_value : in     Vkm_Bool) return Vkm_Bvec2 is
        (GBT.Make_GenType(Last_Index => 1, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    --< @brief
    --< Produces a vector by copying components from an existing vector.
    --<
    --< @param vec2_value 
    --< The 2D vector to copy components from.
    --<
    --< @returns 
    --< A 2D vector with all of its components set equal to the corresponding
    --< components of vec2_value.
    ----------------------------------------------------------------------------
    function Make_Bvec2 (vec2_value : in     Vkm_Bvec2) return Vkm_Bvec2 is
        (GBT.Make_GenType(vec2_value.x,vec2_value.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @brief
    --< Produces a vector by specifying the values for each of its components.
    --
    --< @param value1 
    --< Value for component 1 of the constructed vector.
    --<
    --< @param value2 
    --< Value for component 2 of the constructed vector.
    --
    --< @return A Vec2 with all components set as specified.
    ----------------------------------------------------------------------------
    function Make_Bvec2 (value1, value2 : in    Vkm_Bool) return Vkm_Bvec2
        renames GBT.Make_GenType;


end Vulkan.Math.Bvec2;
