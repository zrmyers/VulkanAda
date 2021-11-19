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
with Vulkan.Math.GenDType;
use Vulkan.Math.GenDType;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package defines a double precision floating point vector type with 2 
--< components.
--------------------------------------------------------------------------------
package Vulkan.Math.Dvec2 is
    pragma Preelaborate;
    pragma Pure;

    --< A 2 component vector of double-precision floating point values.
    subtype Vkm_Dvec2 is Vkm_GenDType(Last_Index => 1);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a vec2 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Vec2:
    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dvec2 type.
    --<
    --< @description
    --< Produce a default vector with all components set to 0.0.
    --<
    --< @return 
    --< A Vec2 with all components set to 0.0.
    ----------------------------------------------------------------------------
    function Make_Dvec2 return Vkm_Dvec2 is
        (GDT.Make_GenType(Last_Index => 1, value => 0.0)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dvec2 type.
    --<
    --< @description
    --< Produce a vector with all components set to the same value.
    --<
    --< @param scalar_value 
    --< The value to set all components to.
    --<
    --< @returns A Vec2 with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Dvec2 (scalar_value : in     Vkm_Double) return Vkm_Dvec2 is
        (GDT.Make_GenType(Last_Index => 1, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dvec2 type.
    --<
    --< @description
    --< Produce a vector by copying components from an existing vector.
    --<
    --< @param vec2_value 
    --< The vec2 to copy components from.
    --<
    --< @return
    --< A vec2 with all of its components set equal to the corresponding
    --< components of vec2_value.
    ----------------------------------------------------------------------------
    function Make_Dvec2 (vec2_value : in     Vkm_Dvec2) return Vkm_Dvec2 is
        (GDT.Make_GenType(vec2_value.x,vec2_value.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_Dvec2 type.
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
    --< @return 
    --< A Vec2 with all components set as specified.
    ----------------------------------------------------------------------------
    function Make_Dvec2 (value1, value2 : in    Vkm_Double) return Vkm_Dvec2
        renames GDT.Make_GenType;


end Vulkan.Math.Dvec2;
