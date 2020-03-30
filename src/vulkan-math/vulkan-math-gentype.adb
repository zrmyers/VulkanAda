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
-- This package describes a generic Vulkan Math type.
--------------------------------------------------------------------------------
package body Vulkan.Math.GenType is

    function Length (A : in     Vkm_GenType) return Vkm_Length is
    begin
        return A.data'Length;
    end Length;

    ----------------------------------------------------------------------------
    -- Operations on Vkm_GenType
    ----------------------------------------------------------------------------
    function Apply_Func_IV_IV_RV(Left, Right : in     Vkm_GenType) return Vkm_GenType is
        Result : Vkm_GenType(Last_Index => To_Indices(Left.Length));
    begin
        for I in Vkm_Indices'First .. To_Indices(Left.Length) loop
            Result.data(I) := Func(Left.data(I), Right.data(I));
        end loop;
        return Result;
    end Apply_Func_IV_IV_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IS_IV_RV(Left  : in     Base_Type;
                                      Right : in     Vkm_GenType) return Vkm_GenType is
        Result : Vkm_GenType(Last_Index => To_Indices(Right.Length));
    begin
        for I in Vkm_Indices'First .. To_Indices(Right.Length) loop
            Result.data(I) := Func(Left, Right.data(I));
        end loop;
        return Result;
    end Apply_Func_IS_IV_RV;

    ----------------------------------------------------------------------------


    function Apply_Func_IV_IS_RV(Left  : in     Vkm_GenType;
                                      Right : in     Base_Type  ) return Vkm_GenType is
        Result : Vkm_GenType(Last_Index => To_Indices(Left.Length));
    begin
        for I in Vkm_Indices'First .. To_Indices(Left.Length) loop
            Result.data(I) := Func(Left.data(I), Right);
        end loop;
        return Result;
    end Apply_Func_IV_IS_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_RV(A : in     Vkm_GenType) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Indices(A.Length));
    begin
        for I in Vkm_Indices'First .. To_Indices(A.Length) loop
            Result.data(I) := Func(A.data(I));
        end loop;
        return Result;
    end Apply_Func_IV_RV;


    function Apply_Func_IV_OV_RV(IV1 : in     Vkm_GenType;
                                 OV1 :    out Vkm_GenType) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Indices(IV1.Length));
    begin
        for I in Vkm_Indices'First .. To_Indices(IV1.Length) loop
            Result.data(I) := Func(IV1.data(I), OV1.data(I));
        end loop;
        return Result;
    end Apply_Func_IV_OV_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IV_IV_RV(IV1, IV2, IV3 : in     Vkm_GenType) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Indices(IV1.Length));
    begin
        for I in Vkm_Indices'First .. To_Indices(IV1.Length) loop
            Result.data(I) := Func(IV1.data(I),IV2.data(I),IV3.data(I));
        end loop;
        return Result;
    end Apply_Func_IV_IV_IV_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IV_IS_RV(IV1, IV2 : in     Vkm_GenType;
                                    IS1      : in     Base_Type) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Indices(IV1.Length));
    begin
        for I in Vkm_Indices'First .. To_Indices(IV1.Length) loop
            Result.data(I) := Func(IV1.data(I),IV2.data(I),IS1);
        end loop;
        return Result;
    end Apply_Func_IV_IV_IS_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IS_IS_RV(IV1      : in     Vkm_GenType;
                                    IS1, IS2 : in     Base_Type) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Indices(IV1.Length));
    begin
        for I in Vkm_Indices'First .. To_Indices(IV1.Length) loop
            Result.data(I) := Func(IV1.data(I),IS1,IS2);
        end loop;
        return Result;
    end Apply_Func_IV_IS_IS_RV;


end Vulkan.Math.GenType;
