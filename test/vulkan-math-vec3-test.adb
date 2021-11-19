--------------------------------------------------------------------------------
-- MIT License
--
-- Copyright (c) 2021 Zane Myers
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
with Vulkan.Test.Framework;
with Vulkan.Math.Vec3;

use Ada.Text_IO;
use Vulkan.Math.Vec3;
use Vulkan.Test.Framework;

--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package provides tests for single precision floating point Vec3.
--------------------------------------------------------------------------------
package body Vulkan.Math.Vec3.Test is

-- Test Vec3
procedure Test_Vec3 is

    vector_1 : Vkm_Vec3 :=
        Make_Vec3(1.0, 2.0, 3.0);

    vector_2 : Vkm_Vec3 :=
        Make_Vec3;

    vector_3 : Vkm_Vec3 :=
        Make_Vec3(vector_1);
begin

    Put_Line("Testing Vec3 Constructors...");

    Put_Line("vector_1 " & Image(vector_1));
    Assert_Vec3_Equals(vector_1, 1.0, 2.0, 3.0);

    Put_Line("vector_2 " & Image(vector_2));
    Assert_Vec3_Equals(vector_2, 0.0, 0.0, 0.0);

    Put_Line("vector_3 " & Image(vector_3));
    Assert_Vec3_Equals(vector_3, 1.0, 2.0, 3.0);

    Put_Line("Testing '=' operator...");
    Put_Line(" vector_1 != vector_2");
    Assert_Vkm_Bool_Equals(vector_1 = vector_2, False);

    Put_Line(" vector_1 != vector_2");
    Assert_Vkm_Bool_Equals(vector_1 = vector_2, False);

    Put_Line(" vector_1 = vector_3");
    Assert_Vkm_Bool_Equals(vector_1 = vector_3, True);

    Put_Line(" Testing unary '+/-' operator");
    Put_Line(" + vector_1 = " & Image(+ vector_1));
    Assert_Vec3_Equals(+vector_1, 1.0, 2.0, 3.0);

    Put_Line(" - vector_1 = " & Image(- vector_1));
    Assert_Vec3_Equals(-vector_1, -1.0, -2.0, -3.0);

    Put_Line("+ ( - vector_1) = " & Image(+(- vector_1)));
    Assert_Vec3_Equals(-vector_1, -1.0, -2.0, -3.0);

    Put_Line("Testing 'abs' operator...");
    Put_Line(" abs( - vector_1) = " & Image(abs(-vector_1)));
    Assert_Vec3_Equals(abs(-vector_1), 1.0, 2.0, 3.0);

    Put_Line("Testing '+' operator...");
    Put_Line(" vector_1 + vector_3 = " & Image(vector_1 + vector_3));
    Assert_Vec3_Equals(vector_1 + vector_3, 2.0, 4.0, 6.0);

    Put_Line("Testing '-' operator...");
    Put_Line(" vector_1 - vector_3 = " & Image(-vector_1 -vector_3));
    Assert_Vec3_Equals(-vector_1 -vector_3, -2.0, -4.0, -6.0);

    Put_Line("Testing '*' operator...");
    Put_Line(" vector_1 * vector_3 = " & Image(vector_1 * vector_3));
    Assert_Vec3_Equals(vector_1 * vector_3, 1.0, 4.0, 9.0);

    --Put_Line(" mat4 * vec1 = " & Image(mat4 * vec1));

end Test_Vec3;

end Vulkan.Math.Vec3.Test;
