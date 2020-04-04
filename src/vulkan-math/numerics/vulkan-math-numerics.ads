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
-- This package instantiates Ada generic numerical operations for use by the 
-- Vulkan Math Library.
--------------------------------------------------------------------------------
with Ada.Numerics.Generic_Elementary_Functions;

package Vulkan.Math.Numerics is
    pragma Preelaborate;
    pragma Pure;
    
    --Instantiation of Generic Elementary Functions for Float.
    package VKM_FLT_NEF is new 
        Ada.Numerics.Generic_Elementary_Functions(Float_Type => Vkm_Float);
    
    -- Instantiation of Generic Elemantry Functions for Double.
    package VKM_DBL_NEF is new
        Ada.Numerics.Generic_Elementary_Functions(Float_Type => Vkm_Double);
        
    ----------------------------------------------------------------------------
    -- @brief
    -- Algorithm for computing the "modf" for a floating point type.
    --
    -- This separates the floating point into its integer part and its fraction
    -- part.
    --
    -- @param[in]     x Mixed integer and fraction number.
    -- @param[out]    i The integer part of 'x'.
    --
    -- @return The fraction part of 'x'
    ----------------------------------------------------------------------------
    generic
        type Floating_Point is digits <>;
    function Compute_Modf (x : in     Floating_Point;
                           i :    out Floating_Point) return Floating_Point;
                      
    ----------------------------------------------------------------------------
    -- @brief
    -- Algorithm for computing a smooth step function.
    --
    -- Computes:
    --    t = Clamp((x - edge0) / (edge1 - edge0), 0, 1)
    --    t = t * t * (3 - 2 * t)
    --
    -- @param[in]     edge0 The first edge to interpolate between.
    -- @param[in]     edge1 The second edge to interpolate between.
    -- @param[in]     x     The value to apply the step function to.
    --
    -- @returns t
    ----------------------------------------------------------------------------
    generic
        type Floating_Point is digits <>;
        with function Clamp (edge0, edge1, x : in     Floating_Point) return Floating_Point;
    function Smooth_Step (edge0, edge1, x : in     Floating_Point) return Floating_Point;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Determine whether the input holds an Inf. Always returns false in Ada.
    --
    -- @param     x The value to test.
    --
    -- @returns False, always.
    ----------------------------------------------------------------------------
    generic 
        type Floating_Point is digits <>;
    function Is_Inf (x : in     Floating_Point) return Vkm_Bool;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Determine whether the input holds a NaN. Always returns false in Ada.
    --
    -- @param     x The value to test.
    --
    -- @returns False, always.
    ----------------------------------------------------------------------------
    generic 
        type Floating_Point is digits <>;
    function Is_Nan (x : in     Floating_Point) return Vkm_Bool;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Break a floating point value into its significand and exponent parts.
    --
    -- @param[in]     x        The value.
    -- @param[out]    exponent The exponent
    --
    -- @return The significand.
    ----------------------------------------------------------------------------
    generic 
        type Floating_Point is digits <>;
    function Frexp (x        : in     Floating_Point;
                    exponent :    out Vkm_Int) return Floating_Point;
                    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Form a floating point value from its significand and exponent parts.
    --
    -- @param[in]     significand The significand.
    -- @param[in]     exponent    The exponent.
    --
    -- @return x = significand * 2^exponent.
    ----------------------------------------------------------------------------
    generic 
        type Floating_Point is digits <>;
    function Ldexp (x        : in     Floating_Point;
                    exponent : in     Vkm_Int) return Floating_Point;
                    
end Vulkan.Math.Numerics;
