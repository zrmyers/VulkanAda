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
with Ada.Text_IO;
use Ada.Text_IO;

with Vulkan.Math;
use Vulkan.Math;

with Vulkan.Math.Operators;
use Vulkan.Math.Operators;

with Vulkan.Math.Vec4;
use Vulkan.Math.Vec4;

With Vulkan.Math.Vec2;
use Vulkan.Math.vec2;


procedure Vulkan_Test.Math is 
    
    
    x, y, z : Vkm_Vec4 := Make_Vec4(scalar_value => 1.0);

    l,m : Vkm_Vec2 := Make_Vec2(scalar_value => 2.0);

    
begin

    Put_Line("Hello World!");
    Put_Line("z.y: " & Vkm_Float'Image(z.y));
    Put_Line("x.x + y.Z: " & Vkm_Float'Image(x.X + y.z));
    Put_Line("x.x: " & Vkm_Float'Image(x.x));
    Put_Line("y.z: " & Vkm_Float'Image(y.z));
    z.y(x.x + y.z);
    Put_Line("z.y := x.x + y.z" & Vkm_Float'Image(z.y));    
    Put_Line(Vkm_Length'Image(z.Length));
    Put_Line("m : " & m.Image);
    m.x(m.x * 2.0);               
    Put_Line("m: " & m.Image);
    Put_Line("x : " & x.Image);
    x := m & m;
    Put_Line("x:= m & m " & x.Image);
    z := 1.0 & 2.0 & 3.0 & 4.0;
    Put_Line("z = " & z.Image);
    y := z + x;
    Put_Line("z + x = " & y.Image);
    
end Vulkan_Test.Math;
