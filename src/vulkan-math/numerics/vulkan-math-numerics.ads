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
with Ada.Numerics.Generic_Elementary_Functions;

--------------------------------------------------------------------------------
--< @group Vulkan Math Numerics
--------------------------------------------------------------------------------
--< @summary
--< This package instantiates Ada generic numerical operations for use by the 
--< Vulkan Math Library.
--------------------------------------------------------------------------------
package Vulkan.Math.Numerics is
    pragma Preelaborate;
    pragma Pure;
    
    --< @private
    --< Instantiation of Generic Elementary Functions for Float.
    package VKM_FLT_NEF is new 
        Ada.Numerics.Generic_Elementary_Functions(Float_Type => Vkm_Float);
    
    --< @private
    --< Instantiation of Generic Elemantry Functions for Double.
    package VKM_DBL_NEF is new
        Ada.Numerics.Generic_Elementary_Functions(Float_Type => Vkm_Double);
        
    ----------------------------------------------------------------------------
    --< @summary
    --< Algorithm for computing the "modf" for a floating point type.
    --<
    --< @description
    --< This operation separates the floating point into its integer part and its 
    --< fraction part.
    --<
    --< @param x 
    --< Mixed integer and fraction number.
    --<
    --< @param i 
    --< The integer part of 'x'.
    --<
    --< @return 
    --< The fraction part of 'x'
    ----------------------------------------------------------------------------
    generic
        type Floating_Point is digits <>;
    function Compute_Modf (x : in     Floating_Point;
                           i :    out Floating_Point) return Floating_Point;

    ----------------------------------------------------------------------------
    --< @summary
    --< Algorithm for computing a smooth step function.
    --<
    --< @description
    --< Computes smooth step as follows:
    --<    t = Clamp((x - edge0) / (edge1 - edge0), 0, 1)
    --<    t = t * t * (3 - 2 * t)
    --<
    --< @param edge0 
    --< The first edge to interpolate between.
    --< 
    --< @param edge1 
    --< The second edge to interpolate between.
    --<
    --< @param x 
    --< The value to apply the step function to.
    --<
    --< @returns
    --< The smooth step function of x.
    ----------------------------------------------------------------------------
    generic
        type Floating_Point is digits <>;
        with function Clamp (edge0, edge1, x : in     Floating_Point) return Floating_Point;
    function Smooth_Step (edge0, edge1, x : in     Floating_Point) return Floating_Point;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds an Inf. 
    --<
    --< @description
    --< Determine whether the input holds an Inf. Always returns false.
    --<
    --< @param x 
    --< The value to test.
    --<
    --< @return 
    --< False, always.
    ----------------------------------------------------------------------------
    generic 
        type Floating_Point is digits <>;
    function Is_Inf (x : in     Floating_Point) return Vkm_Bool;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds a NaN.
    --<
    --< @description
    --< Determine whether the input holds a NaN. Always returns false.
    --<
    --< @param x 
    --< The value to test.
    --<
    --< @returns 
    --< False, always.
    ----------------------------------------------------------------------------
    generic 
        type Floating_Point is digits <>;
    function Is_Nan (x : in     Floating_Point) return Vkm_Bool;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Break a floating point value into its significand and exponent parts.
    --<
    --< @description
    --< Break a floating point value into its significand and exponent parts.
    --<
    --< @param x 
    --< The input parameter..
    --<
    --< @param exponent 
    --< The exponent part of x.
    --<
    --< @return 
    --< The significand part of x.
    ----------------------------------------------------------------------------
    generic 
        type Floating_Point is digits <>;
    function Frexp (x        : in     Floating_Point;
                    exponent :    out Vkm_Int) return Floating_Point;
                    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Form a floating point value from a significand and exponent.
    --<
    --< @description
    --< Form a floating point value from a significand and exponent.
    --<
    --< @param significand
    --< The significand.
    --<
    --< @param exponent
    --< The exponent.
    --<
    --< @return 
    --< x = significand * 2^exponent.
    ----------------------------------------------------------------------------
    generic 
        type Floating_Point is digits <>;
    function Ldexp (significand : in     Floating_Point;
                    exponent    : in     Vkm_Int) return Floating_Point;
                    
end Vulkan.Math.Numerics;
