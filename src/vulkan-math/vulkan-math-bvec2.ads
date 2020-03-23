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
with Vulkan.Math.Vec2_Generic;
with Vulkan.Math.Bool;
use Vulkan.Math.Bool;

package Vulkan.Math.Bvec2 is
    pragma Preelaborate;
    pragma Pure;
    pragma Warnings (Off, "foreign convention function ""To_String"" should not return unconstrained array");
    
    
    package Vkm_Bvec2_Pkg is new Vulkan.Math.Vec2_Generic(
        Vkm_Bool, Vkm_Bool'Image, "-","+","-","abs","*","/","mod","rem","**");
                       
    type Vkm_Bvec2 is new Vkm_Bvec2_Pkg.Vkm_Vec2 with null record;
    
    -- Define Operations specific to a Bvec2
    function "and" (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2;
    function "or"  (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2;
    function "xor" (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2;
    function "not" (Right       : in     Vkm_Bvec2) return Vkm_Bvec2;
    
    -- Component-Wise Equality
    function Equal     (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2;
    function Not_Equal (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2;
    
    -- To Scalar
    function Are_Any  (This        : in     Vkm_Bvec2) return Vkm_Bool;
    function Are_All  (This        : in     Vkm_Bvec2) return Vkm_Bool;
    
    
end Vulkan.Math.Bvec2;
