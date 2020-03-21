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
-- This package proveds math types that can be used to interface with variables
-- that are input or output from shaders via Vulkan.
--
with Interfaces.C;

package Vulkan.Math is
    pragma Preelaborate;
    pragma Pure;
    
    -- A conditional type taking on values of true or false.
    type Vkm_Bool is new Boolean;
    for Vkm_Bool use (
        false => 0,
        true  => 1
    );
    for Vkm_Bool'Size use Interfaces.c.unsigned_char'Size;
    
    -- Scalar operations for the Vkm_Bool Type.
    -- Same as 'not'
    function "-" (Right : Vkm_Bool) return Vkm_Bool;
    -- Identity 
    function "+" (Right : Vkm_Bool) return Vkm_Bool;
    -- Same as 'and'
    function "*" (Left, Right : Vkm_Bool) return Vkm_Bool;
    
    -- A Signed integer.
    type Vkm_Int is new Interfaces.C.int;
    
    -- An unsigned integer.
    type Vkm_Uint is new Interfaces.C.unsigned;
    
    -- A single precision floating point scalar.
    type Vkm_Float is new Interfaces.C.C_Float;
    
    -- A double precision double scalar.
    type Vkm_Double is new Interfaces.C.double;
    
    -- A two component single precision floating point vector.

    -- # vec3
    -- # vec4
    -- # dvec2
    -- # dvec3
    -- # dvec4
    -- # bvec2
    -- # bvec3
    -- # bvec4
    -- # ivec2
    -- # ivec3
    -- # ivec4
    -- # uvec2
    -- # uvec3
    -- # uvec4
    -- # mat2
    -- # mat3
    -- # mat4
    -- # mat2x2
    -- # mat2x3
    -- # mat2x4
    -- # mat3x2
    -- # mat3x3
    -- # mat3x4
    -- # mat4x2
    -- # mat4x3
    -- # mat4x4
    -- # dmat2x2
    -- # dmat2x3
    -- # dmat2x4
    -- # dmat3x2
    -- # dmat3x3
    -- # dmat3x4
    -- # dmat4x2
    -- # dmat4x3
    -- # dmat4x4
end Vulkan.Math;
