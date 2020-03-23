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
with Vulkan.Math.Bvec2;
use Vulkan.Math.Bvec2;

generic
    -- The type of component that makes up the vector. This can be either a 
    -- floating point or a discrete type.
    type Numeric_Type is private;
    with function Image (This        : in     Numeric_Type) return String;
    with function "-"   (Right       : in     Numeric_Type) return Numeric_Type;
    with function "+"   (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function "-"   (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function "abs" (Right       : in     Numeric_Type) return Numeric_Type;
    with function "*"   (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function "/"   (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function "mod" (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function "rem" (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function "**"  (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function "<"   (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function ">"   (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function "<="  (Left, Right : in     Numeric_Type) return Numeric_Type;
    with function ">="  (Left, Right : in     Numeric_Type) return Numeric_Type;
package Vulkan.Math.Vec2_Numeric is
    pragma Preelaborate;
    pragma Pure;
    pragma Warnings (Off, "foreign convention function ""To_String"" should not return unconstrained array");
    
    package Vec2_Numeric_Pkg is new Vulkan.Math.Vec2_Generic(
        Numeric_Type, Image, "-","+","-","abs","*","/","mod","rem","**");
                       
    type Vkm_Vec2_Numeric is new Vec2_Numeric_Pkg.Vkm_Vec2 with null record;
    
    -- Relational Operators
    function "<" (Left, Right : in     Vkm_Vec2_Numeric) return Vkm_BVec2;
    
end Vulkan.Math.Vec2_Numeric;
