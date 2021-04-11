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


    ----------------------------------------------------------------------------


    function Make_GenType (Last_Index : in     Vkm_Indices;
                           value      : in     Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => Last_Index);
    begin
        for I in Vkm_Indices'First .. Instance.Last_Index loop
            Instance.data(I) := value;
        end loop;
        return Instance;
    end Make_GenType;


    ----------------------------------------------------------------------------


    function Make_GenType (value1 : in Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => 0);
    begin
        Instance.data(0) := value1;
        return Instance;
    end Make_GenType;


    ----------------------------------------------------------------------------


    function Make_GenType (value1, value2 : in Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => 1);
    begin
        Instance.data(0) := value1;
        Instance.data(1) := Value2;
        return Instance;
    end Make_GenType;


    ----------------------------------------------------------------------------


    function Make_GenType (value1, value2, value3 : in Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => 2);
    begin
        Instance.data(0) := value1;
        Instance.data(1) := value2;
        Instance.data(2) := value3;
        return Instance;
    end Make_GenType;


    ----------------------------------------------------------------------------


    function Make_GenType (value1, value2, value3, value4 : in Base_Type) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => 3);
    begin
        Instance.data(0) := value1;
        Instance.data(1) := value2;
        Instance.data(2) := value3;
        Instance.data(3) := value4;
        return Instance;
    end Make_GenType;


    ----------------------------------------------------------------------------


    function Make_GenType (value : in     Vkm_GenType) return Vkm_GenType is
        Instance : Vkm_GenType(Last_Index => value.Last_Index);
    begin
        for I in Instance.data'Range loop
            Instance.data(I) := value.data(I);
        end loop;
        return Instance;
    end Make_GenType;


    ----------------------------------------------------------------------------


    procedure Copy (Destination : in out Vkm_GenType;
                    Source      : in     Vkm_GenType;
                    Num_Copy    : in     Vkm_Length;
                    Offset      : in     Vkm_Indices) is
    begin
        for Data_Index in 0 .. To_Vkm_Indices(Num_Copy) loop
            Destination.data(Offset + Data_Index) := Source.data(Data_Index);
        end loop;
    end Copy;


    ----------------------------------------------------------------------------


    function Component (vec   : in     Vkm_GenType;
                        index : in     Vkm_Indices) return Base_Type is
        value : Base_Type := Default_Value;
    begin
        if index <= vec.Last_Index then
            value := vec.data(index);
        end if;
        return value;
    end Component;


    ----------------------------------------------------------------------------


    procedure Component (vec   : in out Vkm_GenType;
                         index : in     Vkm_Indices;
                         value : in     Base_Type) is
    begin
        vec.data (index) := value;
    end Component;


    ----------------------------------------------------------------


    function x (vec1  : in out Vkm_GenType;
                value : in     Base_Type  ) return Vkm_GenType_Reference is
        vec1_access : constant Vkm_GenType_Reference := (Vector => vec1'Unrestricted_Access);
    begin
        x(vec1,value);
        return vec1_access;
    end x;

    procedure x (vec1 : in out Vkm_GenType;
                 value : in    Base_Type  ) is
    begin
        case vec1.Last_Index is
            when 3 => vec1.data(0) := value;
            when 2 => vec1.data(0) := value;
            when 1 => vec1.data(0) := value;
            when 0 => vec1.data(0) := value;
        end case;
    end x;


    ----------------------------------------------------------------


    function y (vec1  : in out Vkm_GenType;
                value : in     Base_Type  ) return Vkm_GenType_Reference is
        vec1_access : constant Vkm_GenType_Reference := (Vector => vec1'Unrestricted_Access);
    begin
        y(vec1, value);
        return vec1_access;
    end y;


    ----------------------------------------------------------------


    procedure y (vec1 : in out Vkm_GenType;
                 value : in    Base_Type  ) is
    begin
        case vec1.Last_Index is
            when 3 => vec1.data(1) := value;
            when 2 => vec1.data(1) := value;
            when 1 => vec1.data(1) := value;
            when 0 => null;
        end case;
    end y;


    function z (vec1  : in out Vkm_GenType;
                value : in     Base_Type  ) return Vkm_GenType_Reference is
        vec1_access : constant Vkm_GenType_Reference := (Vector => vec1'Unrestricted_Access);
    begin
        z(vec1, value);
        return vec1_access;
    end z;


    ----------------------------------------------------------------



    procedure z (vec1 : in out Vkm_GenType;
                 value : in    Base_Type  ) is
    begin
        case vec1.Last_Index is
            when 3 => vec1.data(2) := value;
            when 2 => vec1.data(2) := value;
            when 1 => null;
            when 0 => null;
        end case;
    end z;


    function w (vec1  : in out Vkm_GenType;
                value : in     Base_Type  ) return Vkm_GenType_Reference is
        vec1_access : constant Vkm_GenType_Reference := (Vector => vec1'Unrestricted_Access);
    begin
        w(vec1, value);
        return vec1_access;
    end w;


    ----------------------------------------------------------------



    procedure w (vec1 : in out Vkm_GenType;
                 value : in    Base_Type  ) is
    begin
        case vec1.Last_Index is
            when 3 => vec1.data(3) := value;
            when 2 => null;
            when 1 => null;
            when 0 => null;
        end case;
    end w;


    ----------------------------------------------------------------



    procedure xy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).y(vec2.y);
    end xy;


    ----------------------------------------------------------------


    procedure xz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).z(vec2.y);
    end xz;


    ----------------------------------------------------------------


    procedure xw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).w(vec2.y);
    end xw;


    ----------------------------------------------------------------


    procedure yx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).x(vec2.y);
    end yx;


    ----------------------------------------------------------------


    procedure yz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).z(vec2.y);
    end yz;


    ----------------------------------------------------------------


    procedure yw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).w(vec2.y);
    end yw;


    ----------------------------------------------------------------


    procedure zx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).x(vec2.y);
    end zx;


    ----------------------------------------------------------------


    procedure zy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).y(vec2.y);
    end zy;


    ----------------------------------------------------------------


    procedure zw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).w(vec2.y);
    end zw;


    ----------------------------------------------------------------


    procedure wx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).x(vec2.y);
    end wx;


    ----------------------------------------------------------------


    procedure wy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).y(vec2.y);
    end wy;


    ----------------------------------------------------------------


    procedure wz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).z(vec2.y);
    end wz;


    ----------------------------------------------------------------


    procedure xyz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x)
            .y(vec2.y)
            .z(vec2.z);
    end xyz;


    ----------------------------------------------------------------


    procedure xyw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x)
            .y(vec2.y)
            .w(vec2.z);
    end xyw;


    ----------------------------------------------------------------


    procedure xzy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x)
            .z(vec2.y)
            .y(vec2.z);
    end xzy;


    ----------------------------------------------------------------


    procedure xzw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x)
            .z(vec2.y)
            .w(vec2.z);
    end xzw;


    ----------------------------------------------------------------


    procedure xwy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x)
            .w(vec2.y)
            .y(vec2.z);
    end xwy;


    ----------------------------------------------------------------


    procedure xwz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x)
            .w(vec2.y)
            .z(vec2.z);
    end xwz;


    ----------------------------------------------------------------


    procedure yxz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x)
            .x(vec2.y)
            .z(vec2.z);
    end yxz;


    ----------------------------------------------------------------


    procedure yxw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x)
            .x(vec2.y)
            .w(vec2.z);
    end yxw;


    ----------------------------------------------------------------


    procedure yzx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x)
            .z(vec2.y)
            .x(vec2.z);
    end yzx;


    ----------------------------------------------------------------


    procedure yzw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x)
            .z(vec2.y)
            .w(vec2.z);
    end yzw;


    ----------------------------------------------------------------


    procedure ywx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x)
            .w(vec2.y)
            .x(vec2.z);
    end ywx;


    ----------------------------------------------------------------


    procedure ywz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x)
            .w(vec2.y)
            .z(vec2.z);
    end ywz;


    ----------------------------------------------------------------


    procedure zxy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x)
            .x(vec2.y)
            .y(vec2.z);
    end zxy;


    ----------------------------------------------------------------


    procedure zxw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x)
            .x(vec2.y)
            .w(vec2.z);
    end zxw;


    ----------------------------------------------------------------


    procedure zyx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x)
            .y(vec2.y)
            .x(vec2.z);
    end zyx;


    ----------------------------------------------------------------


    procedure zyw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x)
            .y(vec2.y)
            .w(vec2.z);
    end zyw;


    ----------------------------------------------------------------


    procedure zwx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x)
            .w(vec2.y)
            .x(vec2.z);
    end zwx;


    ----------------------------------------------------------------


    procedure zwy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x)
            .w(vec2.y)
            .y(vec2.z);
    end zwy;


    ----------------------------------------------------------------


    procedure wxy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x)
            .x(vec2.y)
            .y(vec2.z);
    end wxy;


    ----------------------------------------------------------------


    procedure wxz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x)
            .x(vec2.y)
            .z(vec2.z);
    end wxz;


    ----------------------------------------------------------------


    procedure wyx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x)
            .y(vec2.y)
            .x(vec2.z);
    end wyx;


    ----------------------------------------------------------------


    procedure wyz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x)
            .y(vec2.y)
            .z(vec2.z);
    end wyz;


    ----------------------------------------------------------------


    procedure wzx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x)
            .z(vec2.y)
            .x(vec2.z);
    end wzx;


    ----------------------------------------------------------------


    procedure wzy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x)
            .z(vec2.y)
            .y(vec2.z);
    end wzy;


    ----------------------------------------------------------------------------


    procedure xyzw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).y(vec2.y).z(vec2.z).w(vec2.w);
    end xyzw;


    ----------------------------------------------------------------


    procedure xywz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).y(vec2.y).w(vec2.z).z(vec2.w);
    end xywz;


    ----------------------------------------------------------------


    procedure xzyw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).z(vec2.y).y(vec2.z).w(vec2.w);
    end xzyw;


    ----------------------------------------------------------------


    procedure xzwy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).z(vec2.y).w(vec2.z).y(vec2.w);
    end xzwy;


    ----------------------------------------------------------------


    procedure xwyz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).w(vec2.y).y(vec2.z).z(vec2.w);
    end xwyz;


    ----------------------------------------------------------------


    procedure xwzy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.x(vec2.x).w(vec2.y).z(vec2.z).y(vec2.w);
    end xwzy;


    ----------------------------------------------------------------


    procedure yxzw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).x(vec2.y).z(vec2.z).w(vec2.w);
    end yxzw;


    ----------------------------------------------------------------


    procedure yxwz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).x(vec2.y).w(vec2.z).z(vec2.w);
    end yxwz;


    ----------------------------------------------------------------


    procedure yzxw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).z(vec2.y).x(vec2.z).w(vec2.w);
    end yzxw;


    ----------------------------------------------------------------


    procedure yzwx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).z(vec2.y).w(vec2.z).x(vec2.w);
    end yzwx;


    ----------------------------------------------------------------


    procedure ywxz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).w(vec2.y).x(vec2.z).z(vec2.w);
    end ywxz;


    ----------------------------------------------------------------


    procedure ywzx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.y(vec2.x).w(vec2.y).z(vec2.z).x(vec2.w);
    end ywzx;


    ----------------------------------------------------------------


    procedure zxyw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).x(vec2.y).y(vec2.z).w(vec2.w);
    end zxyw;


    ----------------------------------------------------------------


    procedure zxwy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).x(vec2.y).w(vec2.z).y(vec2.w);
    end zxwy;


    ----------------------------------------------------------------


    procedure zyxw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).y(vec2.y).x(vec2.z).w(vec2.w);
    end zyxw;


    ----------------------------------------------------------------


    procedure zywx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).y(vec2.y).w(vec2.z).x(vec2.w);
    end zywx;


    ----------------------------------------------------------------


    procedure zwxy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).w(vec2.y).x(vec2.z).y(vec2.w);
    end zwxy;


    ----------------------------------------------------------------


    procedure zwyx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.z(vec2.x).w(vec2.y).y(vec2.z).x(vec2.w);
    end zwyx;


    ----------------------------------------------------------------


    procedure wxyz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).x(vec2.y).y(vec2.z).z(vec2.w);
    end wxyz;


    ----------------------------------------------------------------


    procedure wxzy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).x(vec2.y).z(vec2.z).y(vec2.w);
    end wxzy;


    ----------------------------------------------------------------


    procedure wyxz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).y(vec2.y).x(vec2.z).z(vec2.w);
    end wyxz;


    ----------------------------------------------------------------


    procedure wyzx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).y(vec2.y).z(vec2.z).x(vec2.w);
    end wyzx;


    ----------------------------------------------------------------


    procedure wzxy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).z(vec2.y).x(vec2.z).y(vec2.w);
    end wzxy;


    ----------------------------------------------------------------


    procedure wzyx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) is
    begin
        vec1.w(vec2.x).z(vec2.y).y(vec2.z).x(vec2.w);
    end wzyx;


    ----------------------------------------------------------------------------


    function Concatenate (Left, Right : in     Vkm_GenType) return Vkm_GenType is
        Result : Vkm_GenType(Left.Last_Index + Right.Last_Index + 1);
    begin
        Result.Copy(Left,Left.Length,0);
        Result.Copy(Right,Right.Length,To_Vkm_Indices(Left.Length + 1));
        return Result;
    end Concatenate;


    ----------------------------------------------------------------------------

    function Equals(left, right : in     Vkm_GenType) return Vkm_Bool is
        are_equal : Vkm_Bool := True;
    begin
        for index in Vkm_Indices'First .. left.last_index loop
            if not (left.data(index) = right.data(index)) then
                are_equal := False;
            end if;
        end loop;
        return are_equal;
    end Equals;


    ----------------------------------------------------------------------------


    function Unary_Minus(instance : in     Vkm_GenType) return Vkm_GenType is
        result : Vkm_GenType(Last_Index => instance.last_index);
    begin
        for index in Vkm_Indices'First .. instance.last_index loop
            result.data(index) := Unary_Minus(instance.data(index));
        end loop;
        return result;
    end Unary_Minus;


    ----------------------------------------------------------------------------


    function Componentwise_Multiply(left, right : in     Vkm_GenType) return Vkm_GenType is
        result : Vkm_GenType(last_index => left.last_index);
    begin
        for index in Vkm_Indices'First .. left.last_index loop
            result.data(index) := Multiply(left.data(index), right.data(index));
        end loop;
        return result;
    end Componentwise_Multiply;


--------------------------------------------------------------------------------


    function Vector_By_Scalar_Multiply(left : in Vkm_GenType;
                                       right : in Base_Type) return Vkm_GenType is
        result : Vkm_GenType(last_index => left.last_index);
    begin
        for index in Vkm_Indices'First .. left.last_index loop
            result.data(index) := Multiply(left.data(index), right);
        end loop;
        return result;
    end Vector_By_Scalar_Multiply;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IV_RV(Left, Right : in     Vkm_GenType) return Vkm_GenType is
        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(Left.Length));
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(Left.Length) loop
            Result.data(I) := Func(Left.data(I), Right.data(I));
        end loop;
        return Result;
    end Apply_Func_IV_IV_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IS_IV_RV(Left  : in     Base_Type;
                                 Right : in     Vkm_GenType) return Vkm_GenType is
        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(Right.Length));
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(Right.Length) loop
            Result.data(I) := Func(Left, Right.data(I));
        end loop;
        return Result;
    end Apply_Func_IS_IV_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IS_RV(Left  : in     Vkm_GenType;
                                 Right : in     Base_Type  ) return Vkm_GenType is
        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(Left.Length));
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(Left.Length) loop
            Result.data(I) := Func(Left.data(I), Right);
        end loop;
        return Result;
    end Apply_Func_IV_IS_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_RV(A : in     Vkm_GenType) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(A.Length));
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(A.Length) loop
            Result.data(I) := Func(A.data(I));
        end loop;
        return Result;
    end Apply_Func_IV_RV;


    function Apply_Func_IV_OV_RV(IV1 : in     Vkm_GenType;
                                 OV1 :    out Vkm_GenType) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(IV1.Length));
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(IV1.Length) loop
            Result.data(I) := Func(IV1.data(I), OV1.data(I));
        end loop;
        return Result;
    end Apply_Func_IV_OV_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IV_IV_RV(IV1, IV2, IV3 : in     Vkm_GenType) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(IV1.Length));
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(IV1.Length) loop
            Result.data(I) := Func(IV1.data(I),IV2.data(I),IV3.data(I));
        end loop;
        return Result;
    end Apply_Func_IV_IV_IV_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IV_IS_RV(IV1, IV2 : in     Vkm_GenType;
                                    IS1      : in     Base_Type) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(IV1.Length));
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(IV1.Length) loop
            Result.data(I) := Func(IV1.data(I),IV2.data(I),IS1);
        end loop;
        return Result;
    end Apply_Func_IV_IV_IS_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IS_IS_RV(IV1      : in     Vkm_GenType;
                                    IS1, IS2 : in     Base_Type) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(IV1.Length));
    begin
        for I in Vkm_Indices'First .. To_Vkm_Indices(IV1.Length) loop
            Result.data(I) := Func(IV1.data(I),IS1,IS2);
        end loop;
        return Result;
    end Apply_Func_IV_IS_IS_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IS_IS_IV_RV(IS1, IS2 : in     Base_Type;
                                    IV1      : in     Vkm_GenType) return Vkm_GenType is

        Result : Vkm_GenType(Last_Index => To_Vkm_Indices(IV1.Length));
    begin
        for I in Result.data'Range loop
            Result.data(I) := Func(IS1,IS2,IV1.data(I));
        end loop;
        return Result;
    end Apply_Func_IS_IS_IV_RV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IV_OV_RV(IV1, IV2 : in     Vkm_GenType;
                                    OV1      :    out Vkm_GenType) return Vkm_GenType is
        result : Vkm_GenType(last_index => IV1.last_index);
    begin
        for index in result.data'Range loop
            result.data(index) := Func(IV1.data(index), IV2.data(index), OV1.data(index));
        end loop;
        return result;
    end Apply_Func_IV_IV_OV_RV;


    ----------------------------------------------------------------------------


    procedure Apply_Func_IV_IV_OV_OV(IV1, IV2 : in     Vkm_GenType;
                                     OV1, OV2 :    out Vkm_GenType) is
    begin
        for index in IV1.data'Range loop
            Func(IV1.data(index), IV2.data(index),
                 OV1.data(index), OV2.data(index));
        end loop;
    end Apply_Func_IV_IV_OV_OV;


    ----------------------------------------------------------------------------


    function Apply_Func_IV_IV_IS_IS_RV(
        IV1, IV2 : in     Vkm_GenType;
        IS1, IS2 : in     Base_Type  ) return Vkm_GenType is

        result : Vkm_GenType(last_index => IV1.last_index);
    begin
        for index in IV1.data'Range loop
            result.data(index) := Func(IV1.data(index),
                                       IV2.data(index),
                                       IS1,
                                       IS2);
        end loop;
        return result;
    end Apply_Func_IV_IV_IS_IS_RV;


end Vulkan.Math.GenType;
