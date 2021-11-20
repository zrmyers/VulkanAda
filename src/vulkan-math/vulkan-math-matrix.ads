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
with Vulkan.Math.GenFMatrix;
with Vulkan.Math.GenDMatrix;
with Vulkan.Math.GenFType;
with Vulkan.Math.GenDType;
with Vulkan.Math.Mat2x2;
with Vulkan.Math.Mat3x3;
with Vulkan.Math.Mat4x4;
with Vulkan.Math.Dmat2x2;
with Vulkan.Math.Dmat3x3;
with Vulkan.Math.Dmat4x4;

use Vulkan.Math.GenFMatrix;
use Vulkan.Math.GenDMatrix;
use Vulkan.Math.GenFType;
use Vulkan.Math.GenDType;
use Vulkan.Math.Mat2x2;
use Vulkan.Math.Mat3x3;
use Vulkan.Math.Mat4x4;
use Vulkan.Math.Dmat2x2;
use Vulkan.Math.Dmat3x3;
use Vulkan.Math.Dmat4x4;

--------------------------------------------------------------------------------
--< @group Vulkan Math Functions
--------------------------------------------------------------------------------
--< @summary
--< This package provides GLSL Matrix Built-in functions.
--<
--< @description
--< All matrix functions operate on matrices of single-point or double-point
--< floating point numbers.
--------------------------------------------------------------------------------
package Vulkan.Math.Matrix is
    --pragma Preelaborate;
    --pragma Pure;


--------------------------------------------------------------------------------
--< @summary
--< Multiply two matrices component-wise.
--<
--< @description
--< Multiply matrix x by matrix y component-wise, ie, result[i][j] is the
--< scalar product of x[i][j] and y[i][j].
--<
--< @x
--< Matrix x, must have same dimmensions as y.
--<
--< @y
--< Matrix y, must have same dimmensions as z
--<
--< @return
--< The component-wise product of two matrices.
--------------------------------------------------------------------------------
function Matrix_Comp_Mult is new GFM.Apply_Func_IM_IM_RM("*");


--------------------------------------------------------------------------------
--< @summary
--< Multiply two matrices component-wise.
--<
--< @description
--< Multiply matrix x by matrix y component-wise, ie, result[i][j] is the
--< scalar product of x[i][j] and y[i][j].
--<
--< @x
--< Matrix x, must have same dimmensions as y.
--<
--< @y
--< Matrix y, must have same dimmensions as z
--<
--< @return
--< The component-wise product of two matrices.
--------------------------------------------------------------------------------
function Matrix_Comp_Mult is new GDM.Apply_Func_IM_IM_RM("*");


--------------------------------------------------------------------------------
--< @summary
--< Perform the outer product of two vectors, generating a matrix.
--<
--< @description
--< Treats the first parameter c as a column vector (matrix with one column)
--< and the second parameter as a row vector (matrix with one row) and does
--< a linear algebraic matrix multiply c * r, yielding a matrix whose number
--< of rows is the number of components in c and whose number of columns is
--< the number of components in r.
--<
--< @param c
--< Column vector, left.
--<
--< @param r
--< Row vector, right.
--<
--< @return
--< Matrix resulting from outer product of left and right vectors.
--------------------------------------------------------------------------------
function Outer_Product(
    c : in     Vkm_GenFType;
    r : in     Vkm_GenFType) return Vkm_Mat;

--------------------------------------------------------------------------------
--< @summary
--< Perform the outer product of two vectors, generating a matrix.
--<
--< @description
--< Treats the first parameter c as a column vector (matrix with one column)
--< and the second parameter as a row vector (matrix with one row) and does
--< a linear algebraic matrix multiply c * r, yielding a matrix whose number
--< of rows is the number of components in c and whose number of columns is
--< the number of components in r.
--<
--< @param c
--< Column vector, left.
--<
--< @param r
--< Row vector, right.
--<
--< @return
--< Matrix resulting from outer product of left and right vectors.
--------------------------------------------------------------------------------
function Outer_Product(
    c : in     Vkm_GenDType;
    r : in     Vkm_GenDType) return Vkm_Dmat;


--------------------------------------------------------------------------------
--< @summary
--< Perform the transpose of a matrix.
--<
--< @description
--< Returns a matrix that is the transpose of m. The input matrix m is not modified.
--<
--< @param m
--< The matrix of which to take the transpose.
--<
--< @return
--< The transpose of matrix m.
--------------------------------------------------------------------------------
function Transpose(
    m : in     Vkm_Mat) return Vkm_Mat;


--------------------------------------------------------------------------------
--< @summary
--< Perform the transpose of a matrix.
--<
--< @description
--< Returns a matrix that is the transpose of m. The input matrix m is not modified.
--<
--< @param m
--< The matrix of which to take the transpose.
--<
--< @return
--< The transpose of matrix m.
--------------------------------------------------------------------------------
function Transpose(
    m : in     Vkm_Dmat) return Vkm_Dmat;


--------------------------------------------------------------------------------
--< @summary
--< Find the determinant of the 2x2 matrix.
--<
--< @description
--< Use Laplace Expansion to find the determinant.
--<
--------------------------------------------------------------------------------
function Determinant2x2(
    m : in     Vkm_Mat2x2) return Vkm_Float is
    (m.c0r0 * m.c1r1 - m.c1r0 * m.c0r1) with Inline;


--------------------------------------------------------------------------------
--< @summary
--< Find the determinant of the 2x2 matrix.
--<
--< @description
--< Use Laplace Expansion to find the determinant.
--<
--------------------------------------------------------------------------------
function Determinant2x2(
    m : in     Vkm_Dmat2x2) return Vkm_Double is
    (m.c0r0 * m.c1r1 - m.c1r0 * m.c0r1) with Inline;


--------------------------------------------------------------------------------
--< @summary
--< Find the determinant of the 3x3 matrix.
--<
--< @description
--< Use Laplace Expansion to find the determinant. Factored to minimize the total
--< number of operations.
--<
--------------------------------------------------------------------------------
function Determinant3x3(
    m : in     Vkm_Mat3x3) return Vkm_Float is
    (  m.c0r0 * ( m.c1r1 * m.c2r2 - m.c2r1 * m.c1r2)
     - m.c1r0 * ( m.c0r1 * m.c2r2 - m.c2r1 * m.c0r2)
     + m.c2r0 * ( m.c0r1 * m.c1r2 - m.c1r1 * m.c0r2)) with Inline;


--------------------------------------------------------------------------------
--< @summary
--< Find the determinant of the 3x3 matrix.
--<
--< @description
--< Use Laplace Expansion to find the determinant. Factored to minimize the total
--< number of operations.
--<
--------------------------------------------------------------------------------
function Determinant3x3(
    m : in     Vkm_Dmat3x3) return Vkm_Double is
    (  m.c0r0 * ( m.c1r1 * m.c2r2 - m.c2r1 * m.c1r2)
     - m.c1r0 * ( m.c0r1 * m.c2r2 - m.c2r1 * m.c0r2)
     + m.c2r0 * ( m.c0r1 * m.c1r2 - m.c1r1 * m.c0r2)) with Inline;


--------------------------------------------------------------------------------
--< @summary
--< Find the determinant of the 4x4 matrix.
--<
--< @description
--< Use Laplace Expansion to find the determinant of the matrix. Memoization is
--< used to reduce the total number of operations performed.
--<
--------------------------------------------------------------------------------
function Determinant4x4(
    m : in     Vkm_Mat4x4) return Vkm_Float;


--------------------------------------------------------------------------------
--< @summary
--< Find the determinant of the 4x4 matrix.
--<
--< @description
--< Use Laplace Expansion to find the determinant of the matrix. Memoization is
--< used to reduce the total number of operations performed.
--<
--------------------------------------------------------------------------------
function Determinant4x4(
    m : in     Vkm_Dmat4x4) return Vkm_Double;


--------------------------------------------------------------------------------
--< @summary
--< Find the inverse of the 2x2 matrix.
--<
--< @description
--< Find the inverse of the 2x2 matrix.
--------------------------------------------------------------------------------
function Inverse2x2(
    m : in     Vkm_Mat2x2) return Vkm_Mat2x2;


--------------------------------------------------------------------------------
--< @summary
--< Find the inverse of the 2x2 matrix.
--<
--< @description
--< Find the inverse of the 2x2 matrix.
--------------------------------------------------------------------------------
function Inverse2x2(
    m : in     Vkm_Dmat2x2) return Vkm_Dmat2x2;


--------------------------------------------------------------------------------
--< @summary
--< Find the inverse of the 3x3 matrix.
--<
--< @description
--< Find the inverse of the 3x3 matrix.
--------------------------------------------------------------------------------
function Inverse3x3(
    m : in     Vkm_Mat3x3) return Vkm_Mat3x3;


--------------------------------------------------------------------------------
--< @summary
--< Find the inverse of the 3x3 matrix.
--<
--< @description
--< Find the inverse of the 3x3 matrix.
--------------------------------------------------------------------------------
function Inverse3x3(
    m : in     Vkm_Dmat3x3) return Vkm_Dmat3x3;


--------------------------------------------------------------------------------
--< @summary
--< Find the inverse of the 4x4 matrix.
--<
--< @description
--< Find the inverse of the 4x4 matrix.
--------------------------------------------------------------------------------
function Inverse4x4(
    m : in     Vkm_Mat4x4) return Vkm_Mat4x4;


--------------------------------------------------------------------------------
--< @summary
--< Find the inverse of the 4x4 matrix.
--<
--< @description
--< Find the inverse of the 4x4 matrix.
--------------------------------------------------------------------------------
function Inverse4x4(
    m : in     Vkm_Dmat4x4) return Vkm_Dmat4x4;


end Vulkan.Math.Matrix;
