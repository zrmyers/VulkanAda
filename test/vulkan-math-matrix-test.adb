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
--------------------------------------------------------------------------------
--< @group Vulkan Math Basic Types
--------------------------------------------------------------------------------
--< @summary
--< This package tests built-in GLS matrix functions
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Vulkan.Math.GenFMatrix;
with Vulkan.Math.Mat2x2;
with Vulkan.Math.Dmat2x2;
with Vulkan.Math.Dmat3x3;
with Vulkan.Math.Mat4x3;
with Vulkan.Math.Dmat4x4;
with Vulkan.Math.Dmat4x3;
with Vulkan.Test.Framework;
with Vulkan.Math.Vec2;
with Vulkan.Math.Vec3;
with Vulkan.Math.Vec4;
with Vulkan.Math.Dvec2;
with Vulkan.Math.Dvec3;
with Vulkan.Math.Dvec4;

use Vulkan.Math.Mat2x2;
use Vulkan.Math.Dmat2x2;
use Vulkan.Math.Dmat3x3;
use Vulkan.Math.Mat4x3;
use Vulkan.Math.Dmat4x3;
use Vulkan.Math.Dmat4x4;
use Vulkan.Math.Vec2;
use Vulkan.Math.Vec3;
use Vulkan.Math.Vec4;
use Vulkan.Math.Dvec2;
use Vulkan.Math.Dvec3;
use Vulkan.Math.Dvec4;
use Vulkan.Math.GenFMatrix;
use Vulkan.Test.Framework;

package body Vulkan.Math.Matrix.Test is

procedure Test_Matrix_Comp_Mult;
procedure Test_Matrix_Outer_Product;
procedure Test_Matrix_Transpose;
procedure Test_Matrix_Determinant;
procedure Test_Matrix_Inversion;

--------------------------------------------------------------------------------
-- Public Operations
--------------------------------------------------------------------------------
-- Test Harness for Mat2x2 regression tests
procedure Test_Matrix_Functions is

begin
    Test_Matrix_Comp_Mult;

    Test_Matrix_Outer_Product;

    Test_Matrix_Transpose;

    Test_Matrix_Determinant;

    Test_Matrix_Inversion;
end Test_Matrix_Functions;


--------------------------------------------------------------------------------


procedure Test_Matrix_Comp_Mult is
    left : constant Vkm_Mat2x2 := Make_Mat2x2(1.0, 2.0,
                                     3.0, 4.0);

    right : constant Vkm_Mat2x2 := Make_Mat2x2(2.0, 0.5,
                                      2.5, -0.5);

    left1 : constant Vkm_Dmat2x2 := Make_Dmat2x2(1.0, 2.0,
                                      3.0, 4.0);

    right1 : constant Vkm_Dmat2x2 := Make_Dmat2x2(2.0, 0.5,
                                       2.5, -0.5);
begin

    Ada.Text_IO.Put_Line("Matrix_Comp_Mult(left, right) = " &Image( Matrix_Comp_Mult(left, right)));

    Assert_Mat2x2_Equals(Matrix_Comp_Mult(left, right), 2.0, 1.0, 7.5, -2.0);

    Ada.Text_IO.Put_Line("Matrix_Comp_Mult(left, right) = " &Image( Matrix_Comp_Mult(left1, right1)));

    Assert_DMat2x2_Equals(Matrix_Comp_Mult(left1, right1), 2.0, 1.0, 7.5, -2.0);
end Test_Matrix_Comp_Mult;


--------------------------------------------------------------------------------


procedure Test_Matrix_Outer_Product is

    left : constant Vkm_Vec2 := Make_Vec2(1.0, 2.0);

    right : constant Vkm_Vec2 := Make_Vec2(3.0, 4.0);

    left1 : constant Vkm_Vec3 := Make_Vec3(6.0, 5.0, 6.0);

    right1 : constant Vkm_Vec4 := Make_Vec4(7.0, 8.0, 9.0, 10.0);

    left_d : constant Vkm_Dvec2 := Make_Dvec2(1.0, 2.0);

    right_d : constant Vkm_Dvec2 := Make_Dvec2(3.0, 4.0);

    left1_d : constant Vkm_Dvec3 := Make_Dvec3(6.0, 5.0, 6.0);

    right1_d : constant Vkm_Dvec4 := Make_Dvec4(7.0, 8.0, 9.0, 10.0);

begin

    Ada.Text_IO.Put_Line("Outer_Prduct(left, right) = " & Image( Outer_Product(left, right)));

    Assert_Mat2x2_Equals(Outer_Product(left, right), 3.0, 4.0, 6.0, 8.0);

    Ada.Text_IO.Put_Line("Outer_Prduct(left1, right1) = " & Image( Outer_Product(left1, right1)));

    Assert_Mat3x4_Equals(Outer_Product(left1, right1), 42.0, 48.0, 54.0, 60.0,
                                                       35.0, 40.0, 45.0, 50.0,
                                                       42.0, 48.0, 54.0, 60.0);

    Ada.Text_IO.Put_Line("Outer_Prduct(left_d, right_d) = " & Image( Outer_Product(left_d, right_d)));

    Assert_Dmat2x2_Equals(Outer_Product(left_d, right_d), 3.0, 4.0, 6.0, 8.0);

    Ada.Text_IO.Put_Line("Outer_Prduct(left1_d, right1_d) = " & Image( Outer_Product(left1_d, right1_d)));

    Assert_Dmat3x4_Equals(Outer_Product(left1_d, right1_d), 42.0, 48.0, 54.0, 60.0,
                                                       35.0, 40.0, 45.0, 50.0,
                                                       42.0, 48.0, 54.0, 60.0);


end Test_Matrix_Outer_Product;


--------------------------------------------------------------------------------


procedure Test_Matrix_Transpose is

    mat : constant Vkm_Mat2x2 := Make_Mat2x2(0.0, 1.0,
                                    2.0, 3.0);

    mat1 : constant Vkm_Mat4x3 := Make_Mat4x3(0.0, 1.0, 2.0,
                                     3.0, 4.0, 5.0,
                                     6.0, 7.0, 8.0,
                                     9.0, 10.0, 11.0);

    mat_d : constant Vkm_Dmat2x2 := Make_Dmat2x2(0.0, 1.0,
                                    2.0, 3.0);

    mat1_d : constant Vkm_Dmat4x3 := Make_Dmat4x3(0.0, 1.0, 2.0,
                                     3.0, 4.0, 5.0,
                                     6.0, 7.0, 8.0,
                                     9.0, 10.0, 11.0);
begin

    Ada.Text_IO.Put_Line("Transpose(mat) = " & Image( Transpose(mat)));

    Assert_Mat2x2_Equals(Transpose(mat), 0.0, 2.0, 1.0, 3.0);

    Ada.Text_IO.Put_Line("Transpose(mat1) = " & Image( Transpose(mat1)));

    Assert_Mat3x4_Equals(Transpose(mat1), 0.0, 3.0, 6.0, 9.0,
                                         1.0, 4.0, 7.0, 10.0,
                                         2.0, 5.0, 8.0, 11.0);

    Ada.Text_IO.Put_Line("Transpose(Transpose(mat1)) = " & Image( Transpose(Transpose(mat1))));

    Assert_Mat4x3_Equals(Transpose(Transpose(mat1)),
                            0.0, 1.0, 2.0,
                            3.0, 4.0, 5.0,
                            6.0, 7.0, 8.0,
                            9.0, 10.0, 11.0);

    Ada.Text_IO.Put_Line("Transpose(mat_d) = " & Image( Transpose(mat_d)));

    Assert_Dmat2x2_Equals(Transpose(mat_d), 0.0, 2.0, 1.0, 3.0);

    Ada.Text_IO.Put_Line("Transpose(mat1_d) = " & Image( Transpose(mat1_d)));

    Assert_Dmat3x4_Equals(Transpose(mat1_d), 0.0, 3.0, 6.0, 9.0,
                                         1.0, 4.0, 7.0, 10.0,
                                         2.0, 5.0, 8.0, 11.0);

    Ada.Text_IO.Put_Line("Transpose(Transpose(mat1_d)) = " & Image( Transpose(Transpose(mat1_d))));

    Assert_Dmat4x3_Equals(Transpose(Transpose(mat1_d)),
                            0.0, 1.0, 2.0,
                            3.0, 4.0, 5.0,
                            6.0, 7.0, 8.0,
                            9.0, 10.0, 11.0);
end Test_Matrix_Transpose;


procedure Test_Matrix_Determinant is

    mat : constant Vkm_Mat2x2 := Make_Mat2x2(0.0, 1.0,
                                    2.0, 3.0);

    mat1 : constant Vkm_Mat3x3 := Make_Mat3x3(1.0, 1.0, 2.0,
                                     3.0, 4.0, 5.0,
                                     6.0, 7.0, 8.0);

    mat2 : constant Vkm_Mat4x4 := Make_Mat4x4(2.0, 1.0, 2.0, 3.0,
                                     3.0, 4.0, 5.0, 4.0,
                                     6.0, 7.0, 8.0, 5.0,
                                     9.0, 8.0, 7.0, 6.0);

    mat_d : constant Vkm_Dmat2x2 := Make_Dmat2x2(0.0, 1.0,
                                    2.0, 3.0);

    mat1_d : constant Vkm_Dmat3x3 := Make_Dmat3x3(1.0, 1.0, 2.0,
                                     3.0, 4.0, 5.0,
                                     6.0, 7.0, 8.0);

    mat2_d : constant Vkm_Dmat4x4 := Make_Dmat4x4(2.0, 1.0, 2.0, 3.0,
                                     3.0, 4.0, 5.0, 4.0,
                                     6.0, 7.0, 8.0, 5.0,
                                     9.0, 8.0, 7.0, 6.0);
begin

    Ada.Text_IO.Put_Line("Determinant(mat)  = " & Determinant2x2(mat)'Image);
    Assert_Vkm_Bool_Equals(Vkm_Bool(Determinant2x2(mat) = -2.0), True);
    Ada.Text_IO.Put_Line("Determinant(mat1) = " & Determinant3x3(mat1)'Image);
    Assert_Vkm_Bool_Equals(Vkm_Bool(Determinant3x3(mat1) = -3.0), True);
    Ada.Text_IO.Put_Line("Determinant(mat2) = " & Determinant4x4(mat2)'Image);
    Assert_Vkm_Bool_Equals(Vkm_Bool(Determinant4x4(mat2) = -36.0), True);

    Ada.Text_IO.Put_Line("Determinant(mat_d)  = " & Determinant2x2(mat_d)'Image);
    Assert_Vkm_Bool_Equals(Vkm_Bool(Determinant2x2(mat_d) = -2.0), True);
    Ada.Text_IO.Put_Line("Determinant(mat1) = " & Determinant3x3(mat1_d)'Image);
    Assert_Vkm_Bool_Equals(Vkm_Bool(Determinant3x3(mat1_d) = -3.0), True);
    Ada.Text_IO.Put_Line("Determinant(mat2) = " & Determinant4x4(mat2_d)'Image);
    Assert_Vkm_Bool_Equals(Vkm_Bool(Determinant4x4(mat2_d) = -36.0), True);

end Test_Matrix_Determinant;


procedure Test_Matrix_Inversion is

    mat : constant Vkm_Mat2x2 := Make_Mat2x2(0.0, 1.0,
                                    2.0, 3.0);

    mat1 : constant Vkm_Mat3x3 := Make_Mat3x3(1.0, 1.0, 2.0,
                                     3.0, 4.0, 5.0,
                                     6.0, 7.0, 8.0);

    mat2 : constant Vkm_Mat4x4 := Make_Mat4x4(2.0, 1.0, 2.0, 3.0,
                                     3.0, 4.0, 5.0, 4.0,
                                     6.0, 7.0, 8.0, 5.0,
                                     9.0, 8.0, 7.0, 6.0);

    mat_d : constant Vkm_Dmat2x2 := Make_Dmat2x2(0.0, 1.0,
                                    2.0, 3.0);

    mat1_d : constant Vkm_Dmat3x3 := Make_Dmat3x3(1.0, 1.0, 2.0,
                                     3.0, 4.0, 5.0,
                                     6.0, 7.0, 8.0);

    mat2_d : constant Vkm_Dmat4x4 := Make_Dmat4x4(2.0, 1.0, 2.0, 3.0,
                                     3.0, 4.0, 5.0, 4.0,
                                     6.0, 7.0, 8.0, 5.0,
                                     9.0, 8.0, 7.0, 6.0);
begin

    -- Verify that the inverse multiplied by original matrix is the identity matrix.
    Ada.Text_IO.Put_Line("Inverse(mat)*mat  = " & Image(Inverse2x2(mat)*mat));
    Assert_Mat2x2_Equals(Inverse2x2(mat)*mat, 1.0, 0.0, 0.0, 1.0);

    Ada.Text_IO.Put_Line("Inverse(mat1)*mat1  = " & Image(Inverse3x3(mat1)*mat1));
    Assert_Mat3x3_Equals(Inverse3x3(mat1)*mat1, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

    Ada.Text_IO.Put_Line("Inverse(mat2)*mat2  = " & Image(Inverse4x4(mat2)*mat2));
    Assert_Mat4x4_Equals(Inverse4x4(mat2)*mat2,
                         1.0, 0.0, 0.0, 0.0,
                         0.0, 1.0, 0.0, 0.0,
                         0.0, 0.0, 1.0, 0.0,
                         0.0, 0.0, 0.0, 1.0);

    -- Verify that the inverse multiplied by original matrix is the identity matrix.
    Ada.Text_IO.Put_Line("Inverse(mat_d)*mat_d  = " & Image(Inverse2x2(mat_d)*mat_d));
    Assert_Dmat2x2_Equals(Inverse2x2(mat_d)*mat_d, 1.0, 0.0, 0.0, 1.0);

    Ada.Text_IO.Put_Line("Inverse(mat1_d)*mat1_d  = " & Image(Inverse3x3(mat1_d)*mat1_d));
    Assert_Dmat3x3_Equals(Inverse3x3(mat1_d)*mat1_d, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

    Ada.Text_IO.Put_Line("Inverse(mat2_d)*mat2_d  = " & Image(Inverse4x4(mat2_d)*mat2_d));
    Assert_Dmat4x4_Equals(Inverse4x4(mat2_d)*mat2_d,
                         1.0, 0.0, 0.0, 0.0,
                         0.0, 1.0, 0.0, 0.0,
                         0.0, 0.0, 1.0, 0.0,
                         0.0, 0.0, 0.0, 1.0);

end Test_Matrix_Inversion;

end Vulkan.Math.Matrix.Test;
