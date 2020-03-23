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
package body Vulkan.Math.Bvec2 is
    
    -- Define Operations specific to a Bvec2
    function "and" (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2 is
    begin
        return Left * Right;
    end "and";
    
    function "or"  (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2 is
    begin
        return Left + Right;
    end "or";
    
    function "xor" (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2 is
    begin
        return Left - Right;
    end "xor";
    
    function "not" (Right       : in     Vkm_Bvec2) return Vkm_Bvec2 is
    begin
        return -Right;
    end "not";
    
    function Equal    (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2 is
    begin
        return (Vkm_Bool(Left.comp_1 = Right.comp_1) & Vkm_Bool(Left.comp_2 = Right.comp_2));
    end Equal;
    
    function Not_Equal (Left, Right : in     Vkm_Bvec2) return Vkm_Bvec2 is
    begin
        return (Vkm_Bool(Left.comp_1 /= Right.comp_1) & Vkm_Bool(Left.comp_2 /= Right.comp_2));
    end Not_Equal;
    
    function Are_Any (This        : in     Vkm_Bvec2) return Vkm_Bool is
    begin
        return (This.comp_1 or This.comp_2);
    end Are_Any;
    
    function Are_All (This        : in     Vkm_Bvec2) return Vkm_Bool is
    begin
        return (This.comp_1 and This.comp_2);
    end Are_All;
    
    
    
end Vulkan.Math.Bvec2;
