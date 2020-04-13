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
with Vulkan.Math.GenDType;
with Vulkan.Math.Dvec3;
with Vulkan.Math.Dvec2;

use Vulkan.Math.GenDType;
use Vulkan.Math.Dvec3;
use Vulkan.Math.Dvec2;

package Vulkan.Math.Dvec4 is
    pragma Preelaborate;
    pragma Pure;

    -- A 4-compoent floating point vector.
    subtype Vkm_Dvec4 is Vkm_GenDType(Last_Index => 3);

    ----------------------------------------------------------------------------
    -- Ada does not have the concept of constructors in the sense that they exist
    -- in C++.  For this reason, we will instead define multiple methods for
    -- instantiating a Dvec4 here.
    ----------------------------------------------------------------------------
    -- The following are explicit constructors for Dvec4:
    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a default vector with all components set to 0.0.
    --
    -- @returns a Dvec4 with all components set to 0.0.
    ----------------------------------------------------------------------------
    function Make_Dvec4 return Vkm_Dvec4 is
        (GDT.Make_GenType(Last_Index => 3, value => 0.0)) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector with all components set to the same value.
    --
    -- @param[in]     scalar_value The value to set all components to.
    --
    -- @returns A Dvec4 with all components set to scalar_value.
    ----------------------------------------------------------------------------
    function Make_Dvec4 (scalar_value : in     Vkm_Double) return Vkm_Dvec4 is
        (GDT.Make_GenType(Last_Index => 3, value => scalar_value)) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by copying components from an existing vector.
    --
    -- @param[in]     dvec4_value The Dvec4 to copy components from.
    --
    -- @returns A Dvec4 with all of its components set equal to the corresponding
    --          components of dvec4_value.
    ----------------------------------------------------------------------------
    function Make_Dvec4 (dvec4_value : in     Vkm_Dvec4) return Vkm_Dvec4 is
        (GDT.Make_GenType(dvec4_value.data(0),dvec4_value.data(1),
                          dvec4_value.data(2),dvec4_value.data(3))) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by specifying the values for each of its components.
    --
    -- @param[in]     value1 Value for component 1.
    -- @param[in]     value2 Value for component 2.
    -- @param[in]     value3 Value for componetn 3.
    -- @param[in]     Value4 value for component 4.
    --
    -- @return A Dvec4 with all components set as specified.
    ----------------------------------------------------------------------------
    function Make_Dvec4 (value1, value2, value3, value4 : in    Vkm_Double) return Vkm_Dvec4
        renames GDT.Make_GenType;

    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by concatenating a scalar value with a Dvec3.
    --
    -- Dvec4 = [scalar_value, dvec3_value  ]
    --
    -- @param[in]     scalar_value The scalar value to concatenate with the Dvec3.
    -- @param[in]     dvec3_value   The Dvec3 to concatenate to the scalar value.
    --
    -- @returns The instance of Dvec4.
    ----------------------------------------------------------------------------
    function Make_Dvec4 (scalar_value : in     Vkm_Double;
                         dvec3_value   : in     Vkm_Dvec3) return Vkm_Dvec3 is
        (Make_Dvec4(scalar_value,
                    dvec3_value.data(0),
                    dvec3_value.data(1),
                    dvec3_value.data(2))) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Produce a vector by concatenating a Dvec3 with a scalar value.
    --
    -- Dvec4 = [dvec3_value, scalar_value]
    --
    -- @param[in]     dvec3_value   The Dvec3 to concatenate to the scalar value.
    -- @param[in]     scalar_value The scalar value to concatenate to the Dvec3.
    --
    -- @returns The instance of Dvec4.
    ----------------------------------------------------------------------------
    function Make_Dvec4 (dvec3_value   : in     Vkm_Dvec3;
                         scalar_value : in     Vkm_Double) return Vkm_Dvec4 is
        (Make_Dvec4(dvec3_value.data(0),
                    dvec3_value.data(1),
                    dvec3_value.data(2),
                    scalar_value      )) with Inline;


     ---------------------------------------------------------------------------
     -- @brief
     -- Produce a vector by concatenating a Dvec2 with a Dvec2.
     --
     -- Dvec4 = [dvec2_value1, dvec2_value2]
     --
     -- @param[in]     dvec2_value1 The first Dvec2.
     -- @param[in]     dvec2_value2 The second Dvec2.
     --
     -- @returns The instance of Dvec4.
     ---------------------------------------------------------------------------
     function Make_Dvec4 (dvec2_value1, dvec2_value2 : in     Vkm_Dvec2) return Vkm_Dvec4 is
         (Make_Dvec4(dvec2_value1.data(0),dvec2_value1.data(1),
                     dvec2_value2.data(0),dvec2_value2.data(1)));


     ---------------------------------------------------------------------------
     -- @brief
     -- Produce a vector by concatenating two scalar values with a Dvec2.
     --
     -- Dvec4 = [scalar1, scalar2, dvec2_value]
     --
     -- @param[in]     scalar1    First scalar value.
     -- @param[in]     scalar2    Second scalar value.
     -- @param[in]     dvec2_value The Dvec2 value.
     --
     -- @returns The instance of Dvec4.
     ---------------------------------------------------------------------------
     function Make_Dvec4 (scalar1, scalar2 : in     Vkm_Double;
                          dvec2_value       : in     Vkm_Dvec2) return Vkm_Dvec4 is
         (Make_Dvec4(scalar1, scalar2, dvec2_value.data(0),dvec2_value.data(1)));


     ---------------------------------------------------------------------------
     -- @brief
     -- Produce a vector by concatenating two scalar values with a Dvec2.
     --
     -- Dvec4 = [scalar1, dvec2_value, scalar2]
     --
     -- @param[in]     scalar1    First scalar value.
     -- @param[in]     dvec2_value The Dvec2 value.
     -- @param[in]     scalar2    Second scalar value.
     --
     -- @returns The instance of Dvec4.
     ---------------------------------------------------------------------------
     function Make_Dvec4 (scalar1     : in     Vkm_Double;
                          dvec2_value : in     Vkm_Dvec2 ;
                          scalar2     : in     Vkm_Double) return Vkm_Dvec4 is
         (Make_Dvec4(scalar1, dvec2_value.data(0),dvec2_value.data(1), scalar2));


     ---------------------------------------------------------------------------
     -- @brief
     -- Produce a vector by concatenating two scalar values with a Dvec2.
     --
     -- Dvec4 = [dvec2_value, scalar1, scalar2]
     --
     -- @param[in]     dvec2_value The Dvec2 value.
     -- @param[in]     scalar1    First scalar value.
     -- @param[in]     scalar2    Second scalar value.
     --
     -- @returns The instance of Dvec4.
     ---------------------------------------------------------------------------
     function Make_Dvec4 (dvec2_value : in     Vkm_Dvec2 ;
                          scalar1     : in     Vkm_Double;
                          scalar2     : in     Vkm_Double) return Vkm_Dvec4 is
         (Make_Dvec4(dvec2_value.data(0),dvec2_value.data(1), scalar1, scalar2));


end Vulkan.Math.Dvec4;
