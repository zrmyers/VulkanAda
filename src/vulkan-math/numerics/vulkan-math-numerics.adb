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

package body Vulkan.Math.Numerics is


    ----------------------------------------------------------------------------


    function Compute_Modf (x : in     Floating_Point;
                           i :    out Floating_Point) return Floating_Point is
    begin
        i := Floating_Point'Truncation(x);
        return x - i;
    end Compute_Modf;

    function Smooth_Step
        (edge0, edge1, x : in     Floating_Point) return Floating_Point
    is
        t : constant Floating_Point := Clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    begin
        return (t * t * (3.0 - 2.0 * t));
    end Smooth_Step;

    function Is_Inf (x : in     Floating_Point) return Vkm_Bool is
        pragma Unreferenced(x);
    begin
        return False;
    end Is_Inf;


    function Is_Nan (x : in     Floating_Point) return Vkm_Bool is
        pragma Unreferenced(x);
    begin
        return False;
    end Is_Nan;


    function Frexp (x        : in     Floating_Point;
                    exponent :    out Vkm_Int) return Floating_Point is
    begin
        exponent := Floating_Point'Exponent(x);
        return Floating_Point'Fraction(x);
    end Frexp;

    function Ldexp (x        : in     Floating_Point;
                    exponent : in     Vkm_Int) return Floating_Point is
    begin
        return Floating_Point'Compose(x, exponent);
    end Ldexp;

end Vulkan.Math.Numerics;
