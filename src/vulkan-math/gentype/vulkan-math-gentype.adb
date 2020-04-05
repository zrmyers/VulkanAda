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


    ----------------------------------------------------------------------------
    -- Operations on Vkm_GenType
    ----------------------------------------------------------------------------


    function Length (A : in     Vkm_GenType) return Vkm_Length is
    begin
        return A.data'Length;
    end Length;



    function Image (Instance : in     Vkm_GenType) return String is
    begin
        case Instance.Length is
            when 4 =>
                return "[ " & Image(Instance.data(0)) &
                       ", " & Image(Instance.data(1)) &
                       ", " & Image(Instance.data(2)) &
                       ", " & Image(Instance.data(3)) & " ]";
            when 3 =>
                return "[ " & Image(Instance.data(0)) &
                       ", " & Image(Instance.data(1)) &
                       ", " & Image(Instance.data(2)) & " ]";
            when 2 =>
                return "[ " & Image(Instance.data(0)) &
                       ", " & Image(Instance.data(1)) & " ]";
            when 1 =>
                return "[ " & Image(Instance.data(0)) & " ]";
        end case;
    end Image;

    function Make (Last_Index : in     Vkm_Indices;
                   value      : in     Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => Last_Index);
    begin
        for I in Vkm_Indices'First .. Instance.Last_Index loop
            Instance.data(I) := value;
        end loop;
        return Instance;
    end Make;


    function Make (value1 : in Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => 0);
    begin
        Instance.data(0) := value1;
        return Instance;
    end Make;

    function Make (value1, value2 : in Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => 1);
    begin
        Instance.data(0) := value1;
        Instance.data(1) := Value2;
        return Instance;
    end Make;

    function Make (value1, value2, value3 : in Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => 2);
    begin
        Instance.data(0) := value1;
        Instance.data(1) := value2;
        Instance.data(2) := value3;
        return Instance;
    end Make;

    function Make (value1, value2, value3, value4 : in Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => 3);
    begin
        Instance.data(0) := value1;
        Instance.data(1) := value2;
        Instance.data(2) := value3;
        Instance.data(3) := value4;
        return Instance;
    end Make;


    ----------------------------------------------------------------------------


    function Get (Instance : in out Vkm_Access_Component_2D) return Vkm_GenType is
        Result : Vkm_GenType(Last_Index => 1);
    begin
        Result.data(0) := Instance.Data0.all;
        Result.data(1) := Instance.Data1.all;
        return Result;
    end Get;

    procedure Copy (Destination : in out Vkm_GenType;
                    Source      : in     Vkm_GenType;
                    Num_Copy    : in     Vkm_Length;
                    Offset      : in     Vkm_Indices) is
    begin
        for Data_Index in 0 .. To_Indices(Num_Copy) loop
            Destination.data(Offset + Data_Index) := Source.data(Data_Index);
        end loop;
    end Copy;


    ----------------------------------------------------------------------------


    function Concatenate (Left, Right : in     Vkm_GenType) return Vkm_GenType is
        Result : Vkm_GenType(Left.Last_Index + Right.Last_Index + 1);
    begin
        Result.Copy(Left,Left.Length,0);
        Result.Copy(Right,Right.Length,To_Indices(Left.Length));
        return Result;
    end Concatenate;

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
