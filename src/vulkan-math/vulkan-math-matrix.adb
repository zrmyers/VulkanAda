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

package body Vulkan.Math.Matrix is


function Outer_Product(
    c : in     Vkm_GenFType;
    r : in     Vkm_GenFType) return Vkm_Mat is

    result : Vkm_Mat := GFM.Make_GenMatrix(
        cN => To_Vkm_Indices(r.Length),
        rN => To_Vkm_Indices(c.Length));

begin
    for c_iter in Vkm_Indices'First .. result.last_row_index loop
        for r_iter in Vkm_Indices'First .. result.last_column_index loop
            result.data(r_iter, c_iter) := c.data(c_iter)* r.data(r_iter);
        end loop;
    end loop;
    return result;
end Outer_Product;


--------------------------------------------------------------------------------


function Transpose(
    m : in     Vkm_Mat) return Vkm_Mat is

    result : Vkm_Mat := GFM.Make_GenMatrix(
        cN => m.last_row_index,
        rN => m.last_column_index);

begin
    for row_index in Vkm_Indices'First .. m.last_row_index loop
        for col_index in Vkm_Indices'First .. m.last_column_index loop
            result.data(row_index, col_index) := m.data(col_index, row_index);
        end loop;
    end loop;
    return result;
end Transpose;


--------------------------------------------------------------------------------


function Determinant4x4(
    m : in     Vkm_Mat4x4) return Vkm_Float is

    -- Memoization of all 2x2 determinants after expansion on columns 0 and 1.
    subfactor00 : constant Vkm_Float := m.c2r2 * m.c3r3 - m.c3r2 * m.c2r3;
    subfactor01 : constant Vkm_Float := m.c2r1 * m.c3r3 - m.c3r1 * m.c2r3;
    subfactor02 : constant Vkm_Float := m.c2r1 * m.c3r2 - m.c3r1 * m.c2r2;
    subfactor03 : constant Vkm_Float := m.c2r0 * m.c3r3 - m.c3r0 * m.c2r3;
    subfactor04 : constant Vkm_Float := m.c2r0 * m.c3r2 - m.c3r0 * m.c2r2;
    subfactor05 : constant Vkm_Float := m.c2r0 * m.c3r1 - m.c3r0 * m.c2r1;

    -- Laplacian expansion on c1
    detcof0 : constant Vkm_Float := +( m.c1r1 * subfactor00 - m.c1r2 * subfactor01 + m.c1r3 * subfactor02);
    detcof1 : constant Vkm_Float := -( m.c1r0 * subfactor00 - m.c1r2 * subfactor03 + m.c1r3 * subfactor04);
    detcof2 : constant Vkm_Float := +( m.c1r0 * subfactor01 - m.c1r1 * subfactor03 + m.c1r3 * subfactor05);
    detcof3 : constant Vkm_Float := -( m.c1r0 * subfactor02 - m.c1r1 * subfactor04 + m.c1r2 * subfactor05);
begin

    -- Laplactian expansion on c0
    return (m.c0r0 * detcof0 + m.c0r1 * detcof1 + m.c0r2 * detcof2 + m.c0r3 * detcof3);
end Determinant4x4;


--------------------------------------------------------------------------------


function Inverse2x2(
    m : in     Vkm_Mat2x2) return Vkm_Mat2x2 is

    one_over_determinant : constant Vkm_Float := 1.0 / (m.c0r0 * m.c1r1 - m.c0r1 * m.c1r0);
begin
    return Make_Mat2x2( m.c1r1 * one_over_determinant,
                       -m.c0r1 * one_over_determinant,
                        m.c1r0 * one_over_determinant,
                       -m.c0r0 * one_over_determinant);
end Inverse2x2;


--------------------------------------------------------------------------------


function Inverse3x3(
    m : in     Vkm_Mat3x3) return Vkm_Mat3x3 is

    one_over_determinant : constant Vkm_Float := 1.0 /
        (  m.c0r0 * ( m.c1r1 * m.c2r2 - m.c2r1 * m.c1r2)
         - m.c1r0 * ( m.c0r1 * m.c2r2 - m.c2r1 * m.c0r2)
         + m.c2r0 * ( m.c0r1 * m.c1r2 - m.c1r1 * m.c0r2));
begin

    -- Adjunct is the transpose of the matrix of cofactors.
    return Make_Mat3x3( (m.c1r1 * m.c2r2 - m.c1r2 * m.c2r1) * one_over_determinant,
                       -(m.c1r0 * m.c2r2 - m.c1r2 * m.c2r0) * one_over_determinant,
                        (m.c1r0 * m.c2r1 - m.c1r1 * m.c2r0) * one_over_determinant,
                       -(m.c0r1 * m.c2r2 - m.c0r2 * m.c2r1) * one_over_determinant,
                        (m.c0r0 * m.c2r2 - m.c0r2 * m.c2r0) * one_over_determinant,
                       -(m.c0r0 * m.c2r1 - m.c0r1 * m.c2r0) * one_over_determinant,
                        (m.c0r1 * m.c1r2 - m.c0r2 * m.c1r1) * one_over_determinant,
                       -(m.c0r0 * m.c1r2 - m.c0r2 * m.c1r0) * one_over_determinant,
                        (m.c0r0 * m.c1r1 - m.c0r1 * m.c1r0) * one_over_determinant);
end


--------------------------------------------------------------------------------


function Inverse4x4(
    m : in     Vkm_Mat4x4) return Vkm_Mat4x4 is


begin

end Inverse4x4;


end Vulkan.Math.Matrix;
