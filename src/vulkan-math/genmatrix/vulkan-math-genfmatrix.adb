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

package body Vulkan.Math.GenFMatrix is


    ----------------------------------------------------------------------------


    function "*" (
        left  : in     Vkm_Mat;
        right : in     Vkm_GenFType ) return Vkm_GenFType is

        result : Vkm_GenFType := GFT.Make_GenType(
            last_index => left.last_row_index,
            value      => 0.0);

    begin
        for row_index in Vkm_Indices'First .. left.last_row_index loop
            for dot_index in Vkm_Indices'First .. left.last_column_index loop
                result.data(row_index) := result.data(row_index) +
                    (left.data(dot_index, row_index) * right.data(dot_index));
            end loop;
        end loop;
        return result;
    end "*";


    ----------------------------------------------------------------------------


    function "*" (
        left  : in     Vkm_GenFType;
        right : in     Vkm_Mat     ) return Vkm_GenFType is

        result : Vkm_GenFType := GFT.Make_GenType(
            last_index => right.last_column_index,
            value      => 0.0);

    begin
        for col_index in Vkm_Indices'First .. right.last_column_index loop
            for dot_index in Vkm_Indices'First .. right.last_row_index loop
                result.data(col_index) := result.data(col_index) +
                    (left.data(dot_index) * right.data(col_index, dot_index));
            end loop;
        end loop;
        return result;
    end "*";


end Vulkan.Math.GenFMatrix;
