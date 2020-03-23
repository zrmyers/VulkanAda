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
package Vulkan.Math.Ivec2 is
    pragma Preelaborate;
    pragma Pure;
    pragma Warnings (Off, "foreign convention function ""To_String"" should not return unconstrained array");
    
    package Ivec2_Pkg is new Vulkan.Math.Vec2_Numeric(
        Vkm_Bool, Vkm_Bool'Image, "-","+","-","abs","*","/","mod","rem","**");
                       
    type Vkm_Ivec2 is new Ivec2_Pkg.Vkm_Vec2_Numeric with null record;
    
    -- Relational Operators
    function "<" (Left, Right : in     Vkm_Vec2_Numeric) return Vkm_BVec2;
    
end Vulkan.Math.Ivec2;
