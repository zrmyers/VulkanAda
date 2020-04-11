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
-- This package provides GLSL geometry operations.
--------------------------------------------------------------------------------
with Vulkan.Math.GenFType;
with Vulkan.Math.GenDType;
with Vulkan.Math.Operators;
with Vulkan.Math.Vec3;
with Vulkan.Math.Dvec3;

use Vulkan.Math.GenFType;
use Vulkan.Math.GenDType;
use Vulkan.Math.Operators;
use Vulkan.Math.Vec3;
use Vulkan.Math.Dvec3;

package Vulkan.Math.Geometry is
    pragma Preelaborate;
    pragma Pure;

    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate the magnitude of the vector, using the formula:
    --
    --    Magnitude = sqrt(sum(x0^2, ..., xn^2))
    --
    -- @param[in]     x The vector to determine the magnitude for.
    --
    -- @returns The magnitude of the vector.
    ----------------------------------------------------------------------------
    function Mag (x : in     Vkm_GenFType) return Vkm_Float;
    function Mag (x : in     Vkm_GenDType) return Vkm_Double;


    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate the distance between two points, p0 and p1, using the formula:
    --
    --    Distance = Magnitude(p0 - p1)
    --
    -- @param     p0 A vector which represents the first point.
    -- @param     p1 A vector which represents the seconds point.
    --
    -- @return The distance between the two points.
    ----------------------------------------------------------------------------
    function Distance (p0, p1 : in     Vkm_GenFType) return Vkm_Float is
        (Mag(p0 - p1)) with Inline;
    function Distance (p0, p1 : in     Vkm_GenDType) return Vkm_Double is
        (Mag(p0 - p1)) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate the dot product between the two vectors.
    --
    -- x dot y = [x1 ... xN] . | y1  | = x1*y1 + ... xN * yN
    --                         | ... |
    --                         | yN  |
    --
    -- @param[in]     x The left vector in the dot product operation.
    -- @param[in]     y The right vector in the dot product operation.
    --
    -- @return The dot product of the two vectors.
    ----------------------------------------------------------------------------
    function Dot (x, y : in     Vkm_GenFType) return Vkm_Float;
    function Dot (x, y : in     Vkm_GenDType) return Vkm_Double;


    ----------------------------------------------------------------------------
    -- @brief
    -- Calculate the cross product between two 3 dimmensional vectors.
    --
    -- x cross y = | i  j  k  | = i | x1 x2 | -j | x0 x2 | +k | x0 x1 | = | +(x1*y2 - x2*y1) |
    --             | x0 x1 x2 |     | y1 y2 |    | y0 y2 |    | y0 y1 |   | -(x0*y2 - x2*y1) |
    --             | y0 y1 y2 |                                           | +(x0*y1 - x1*y0) |
    --
    -- @param[in]     x The left vector in the cross product operation.
    -- @param[in]     y The right vector in the cross product operation.
    --
    -- @return The cross product of the two vectors.
    ----------------------------------------------------------------------------
    function Cross (x, y : in     Vkm_Vec3 ) return Vkm_Vec3;
    function Cross (x, y : in     Vkm_Dvec3) return Vkm_Dvec3;


    ----------------------------------------------------------------------------
    -- @brief
    -- Normalize the input vector so that it has a magnitude of 1.
    --
    -- @param[in]     x The vector to normalize.
    --
    -- @return The normalized vector.
    ----------------------------------------------------------------------------
    function Normalize(x : in     Vkm_GenFType) return Vkm_GenFType is
        (x / Mag(x)) with inline;
    function Normalize(x : in     Vkm_GenDType) return Vkm_GenDType is
        (x / Mag(x)) with inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- Return a normal vector N as-is if an incident vector I points in the opposite
    -- direction of a reference normal vector, Nref.  Otherwise, if I is pointing
    -- in the same direction as the reference normal, flip the normal vector N.
    --
    -- If Nref dot I is negative, these vectors are not facing the same direction.
    -- If Nref dot I is positive, these vectors are facing in the same direction.
    -- If Nref dot I is zero, these two vectors are orthogonal to each other.
    --
    -- @param[in]     n    The normal vector N
    -- @param[in]     i    The incident vector I
    -- @param[in]     nref The reference normal vector Nref
    --
    -- @return If I dot Nref < 0, return N. Otherwise return -N.
    ----------------------------------------------------------------------------
    function Face_Forward(n, i, nref : in     Vkm_GenFType) return Vkm_GenFType is
        (if Dot(nref,i) < 0.0 then n else -n) with Inline;
    function Face_Forward(n, i, nref : in     Vkm_GenDType) return Vkm_GenDType is
        (if Dot(nref,i) < 0.0 then n else -n) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- For the incident vector I and surface orientation N, returns the reflection
    -- direction:
    --
    -- I - 2 * ( N dot I ) * N.
    --
    -- @param[in]     i The incident vector I.
    -- @param[in]     n The normalized normal vector N.
    --
    -- @return The reflection direction.
    ----------------------------------------------------------------------------
    function Reflect(i, n : in     Vkm_GenFType) return Vkm_GenFType is
        (i - 2.0 * Dot(n, i) * n) with Inline;
    function Reflect(i, n : in     Vkm_GenDType) return Vkm_GenDType is
        (i - 2.0 * Dot(n, i) * n) with Inline;


    ----------------------------------------------------------------------------
    -- @brief
    -- For the indident vector I and surface normal N, and the ratio of refraction
    -- eta, return the refraction vector.
    --
    -- k = 1.0 - eta^2 (1.0 - dot(N,I)^2)
    --
    -- If k < 0, the result is a vector of all zeros.
    -- Else    , the result is: eta*I - (eta*dot(N,I) + sqrt(k))*N
    --
    -- @param[in]     i   The incident vector I.
    -- @param[in]     n   The surface normal vector N.
    -- @param[in]     eta The indices of refraction.
    --
    -- @return The refraction vector.
    ----------------------------------------------------------------------------
    function Refract(i, n : in     Vkm_GenFType;
                     eta  : in     Vkm_Float   ) return Vkm_GenFType;
    function Refract(i, n : in     Vkm_GenDType;
                     eta  : in     Vkm_Double  ) return Vkm_GenDType;


end Vulkan.Math.Geometry;
