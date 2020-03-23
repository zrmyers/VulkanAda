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

--------------------------------------------------------------------------------
-- The purpose for this math library is to provide a substitue for the GLM C
-- library for using Vulkan in the Ada Programming language.  Each of the basic
-- types described in the GLSL specification are implemented and tested using
-- native Ada Types.
--------------------------------------------------------------------------------
package Vulkan.Math is
    pragma Preelaborate;
    pragma Pure;
    
    -- Vkm_Vec2 Size 
    type Vkm_Vec2_Size is new Integer range 0 .. 2;
    
    -- Useful access Types
    subtype Vkm_Vec2_Indices is Vkm_Vec2_Size range 0 .. 1;
    type Vkm_Vec2xy is ( x, y );    
    type Vkm_Vec2rg is ( r, g );
    type Vkm_Vec2st is ( s, t );
    
    -- Useful Swizzle access types.
    type Vkm_Vec2xy_Swizzle2 is ( xx, xy, yx, yy);
    type Vkm_Vec2rg_Swizzle2 is ( rr, rg, gr, gg);
    type Vkm_Vec2st_Swizzle2 is ( ss, st, ts, tt);
    
end Vulkan.Math;
