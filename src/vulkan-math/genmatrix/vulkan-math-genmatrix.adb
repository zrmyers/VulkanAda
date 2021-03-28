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

package body Vulkan.Math.GenMatrix is

    --< Used to look up identity matrix values when initializing a matrix.
    IDENTITY_LOOKUP : constant Vkm_Matrix(0 .. 3, 0 .. 3) :=
        (
            (1.0, 0.0, 0.0, 0.0),
            (0.0, 1.0, 0.0, 0.0),
            (0.0, 0.0, 1.0, 0.0),
            (0.0, 0.0, 0.0, 1.0)
        );

    ----------------------------------------------------------------------------
    -- Operations
    ----------------------------------------------------------------------------


    function Image (instance : in     Vkm_GenMatrix) return String is

    begin
        case instance.last_row_index is
            when 3 =>
                return "[ " & Image(instance.r0) &
                       ", " & Image(instance.r1) &
                       ", " & Image(instance.r2) &
                       ", " & Image(instance.r3) & " ]";
            when 2 =>
                return "[ " & Image(Instance.r0) &
                       ", " & Image(Instance.r1) &
                       ", " & Image(Instance.r2) & " ]";
            when 1 =>
                return "[ " & Image(Instance.r0) &
                       ", " & Image(Instance.r1) & " ]";
            when 0 =>
                return "[ " & Image(Instance.r0) & " ]";
        end case;
    end Image;

    ----------------------------------------------------------------------------


    function Element(instance             : in     Vkm_GenMatrix;
                     col_index, row_index : in     Vkm_Indices) return Base_Type is
        value : Base_Type := IDENTITY_LOOKUP(col_index, row_index);
    begin
        if col_index <= instance.last_column_index and
           row_index <= instance.last_row_index then
            value := instance.data(col_index, row_index);
        end if;
        return value;
    end Element;


    ----------------------------------------------------------------------------


    procedure Element(
        instance             : in out Vkm_GenMatrix;
        col_index, row_index : in     Vkm_Indices;
        value                : in     Base_Type) is
    begin
        if col_index <= instance.last_column_index and
           row_index <= instance.last_row_index then

            instance.data(col_index,row_index) := value;
        end if;
    end Element;


    ----------------------------------------------------------------------------



    procedure c0r0(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 0, 0, value);
    end c0r0;

    ----------------------------------------------------------------------------


    procedure c0r1(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 0, 1, value);
    end c0r1;


    ----------------------------------------------------------------------------


    procedure c0r2(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 0, 2, value);
    end c0r2;


    ----------------------------------------------------------------------------


    procedure c0r3(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 0, 3, value);
    end c0r3;


    ----------------------------------------------------------------------------


    procedure c1r0(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 1, 0, value);
    end c1r0;


    ----------------------------------------------------------------------------


    procedure c1r1(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 1, 1, value);
    end c1r1;


    ----------------------------------------------------------------------------


    procedure c1r2(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 1, 2, value);
    end c1r2;


    ----------------------------------------------------------------------------


    procedure c1r3(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 1, 3, value);
    end c1r3;


    ----------------------------------------------------------------------------


    procedure c2r0(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 2, 0, value);
    end c2r0;


    ----------------------------------------------------------------------------


    procedure c2r1(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 2, 1, value);
    end c2r1;


    ----------------------------------------------------------------------------


    procedure c2r2(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 2, 2, value);
    end c2r2;


    ----------------------------------------------------------------------------


    procedure c2r3(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 2, 3, value);
    end c2r3;


    ----------------------------------------------------------------------------


    procedure c3r0(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 3, 0, value);
    end c3r0;


    ----------------------------------------------------------------------------


    procedure c3r1(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 3, 1, value);
    end c3r1;


    ----------------------------------------------------------------------------


    procedure c3r2(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 3, 2, value);
    end c3r2;


    ----------------------------------------------------------------------------


    procedure c3r3(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type) is
    begin
        Element(instance, 3, 3, value);
    end c3r3;


    ----------------------------------------------------------------------------


    function  Element(
        instance             : in out Vkm_GenMatrix;
        col_index, row_index : in     Vkm_Indices;
        value                : in     Base_Type    ) return Vkm_GenMatrix_Reference is

        instance_ref : constant Vkm_GenMatrix_Reference := (instance => instance'Unrestricted_Access);
    begin
        Element(instance, col_index, row_index, value);
        return instance_ref;
    end Element;


    ----------------------------------------------------------------------------


    procedure Column (
        instance  : in out Vkm_GenMatrix;
        col_index : in     Vkm_Indices;
        col_val   : in     Base_Vector_Type) is
    begin
        instance.Element(col_index, 0, x(col_val))
                .Element(col_index, 1, y(col_val))
                .Element(col_index, 2, z(col_val))
                .Element(col_index, 3, w(col_val));
    end Column;


    ----------------------------------------------------------------------------


    procedure c0 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) is
    begin
        Column(instance, 0, col_val);
    end c0;


    ----------------------------------------------------------------------------


    procedure c1 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) is
    begin
        Column(instance, 1, col_val);
    end c1;


    ----------------------------------------------------------------------------

    procedure c2 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) is
    begin
        Column(instance, 2, col_val);
    end c2;


    ----------------------------------------------------------------------------


    procedure c3 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) is
    begin
        Column(instance, 3, col_val);
    end c3;


    ----------------------------------------------------------------------------


    function  Column(
        instance  : in out Vkm_GenMatrix;
        col_index : in     Vkm_Indices  ;
        col_val   : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is

        instance_ref : constant Vkm_GenMatrix_Reference := (instance => instance'Unrestricted_Access);
    begin
        Column(instance, col_index, col_val);
        return instance_ref;
    end Column;


    ----------------------------------------------------------------------------


    procedure Row (
        instance  : in out Vkm_GenMatrix;
        row_index : in     Vkm_Indices;
        row_val   : in     Base_Vector_Type) is
    begin
        instance.Element(0, row_index, x(row_val))
                .Element(1, row_index, y(row_val))
                .Element(2, row_index, z(row_val))
                .Element(3, row_index, w(row_val));
    end Row;


    ----------------------------------------------------------------------------


    procedure r0 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) is
    begin
        Row(instance, 0, row_val);
    end r0;


    ----------------------------------------------------------------------------


    procedure r1 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) is
    begin
        Row(instance, 1, row_val);
    end r1;


    ----------------------------------------------------------------------------


    procedure r2 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) is
    begin
        Row(instance, 2, row_val);
    end r2;

    ----------------------------------------------------------------------------


    procedure r3 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) is
    begin
        Row(instance, 3, row_val);
    end r3;


    ----------------------------------------------------------------------------


    function Row (
        instance  : in out Vkm_GenMatrix;
        row_index : in     Vkm_Indices;
        row_val   : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is

        instance_ref : constant Vkm_GenMatrix_Reference := (instance => instance'Unrestricted_Access);
    begin
        Row(instance, row_index, row_val);
        return instance_ref;
    end Row;


    ----------------------------------------------------------------------------


    function Make_GenMatrix(
        cN, rN                                 : in     Vkm_Indices;
        c0r0_val, c0r1_val, c0r2_val, c0r3_val,
        c1r0_val, c1r1_val, c1r2_val, c1r3_val,
        c2r0_val, c2r1_val, c2r2_val, c2r3_val,
        c3r0_val, c3r1_val, c3r2_val, c3r3_val : in     Base_Type := 0.0) return Vkm_GenMatrix is

        instance : Vkm_GenMatrix(last_column_index => cN, last_row_index => rN);
    begin
        case cN is

            --------------------------------------------------------------------
            -- 1 Column
            --------------------------------------------------------------------
            when 0 =>
                case rN is
                    when 0 => instance.data := (0 => (0 => c0r0_val));

                    when 1 => instance.data := (0 => (c0r0_val, c0r1_val));

                    when 2 => instance.data := (0 => (c0r0_val, c0r1_val, c0r2_val));

                    when 3 => instance.data := (0 => (c0r0_val, c0r1_val, c0r2_val, c0r3_val));
                end case;

            --------------------------------------------------------------------
            -- 2 Columns
            --------------------------------------------------------------------
            when 1 =>
                case rN is
                    when 0 => instance.data := ((0 => c0r0_val),
                                                (0 => c1r0_val));

                    when 1 => instance.data := ((c0r0_val, c0r1_val),
                                                (c1r0_val, c1r1_val));

                    when 2 => instance.data := ((c0r0_val, c0r1_val, c0r2_val),
                                                (c1r0_val, c1r1_val, c1r2_val));

                    when 3 => instance.data := ((c0r0_val, c0r1_val, c0r2_val, c0r3_val),
                                                (c1r0_val, c1r1_val, c1r2_val, c1r3_val));
                end case;

            --------------------------------------------------------------------
            -- 3 Columns
            --------------------------------------------------------------------
            when 2 =>
                case rN is
                    when 0 => instance.data := ((0 => c0r0_val),
                                                (0 => c1r0_val),
                                                (0 => c2r0_val));

                    when 1 => instance.data := ((c0r0_val, c0r1_val),
                                                (c1r0_val, c1r1_val),
                                                (c2r0_val, c2r1_val));

                    when 2 => instance.data := ((c0r0_val, c0r1_val, c0r2_val),
                                                (c1r0_val, c1r1_val, c1r2_val),
                                                (c2r0_val, c2r1_val, c2r2_val));

                    when 3 => instance.data := ((c0r0_val, c0r1_val, c0r2_val, c0r3_val),
                                                (c1r0_val, c1r1_val, c1r2_val, c1r3_val),
                                                (c2r0_val, c2r1_val, c2r2_val, c2r3_val));
                end case;

            --------------------------------------------------------------------
            -- 4 Columns
            --------------------------------------------------------------------
            when 3 =>
                case rN is
                    when 0 => instance.data := ((0 => c0r0_val),
                                                (0 => c1r0_val),
                                                (0 => c2r0_val),
                                                (0 => c2r0_val));

                    when 1 => instance.data := ((c0r0_val, c0r1_val),
                                                (c1r0_val, c1r1_val),
                                                (c2r0_val, c2r1_val),
                                                (c3r0_val, c3r1_val));

                    when 2 => instance.data := ((c0r0_val, c0r1_val, c0r2_val),
                                                (c1r0_val, c1r1_val, c1r2_val),
                                                (c2r0_val, c2r1_val, c2r2_val),
                                                (c3r0_val, c3r1_val, c3r2_val));

                    when 3 => instance.data := ((c0r0_val, c0r1_val, c0r2_val, c0r3_val),
                                                (c1r0_val, c1r1_val, c1r2_val, c1r3_val),
                                                (c2r0_val, c2r1_val, c2r2_val, c2r3_val),
                                                (c3r0_val, c3r1_val, c3r2_val, c3r3_val));
                end case;
            end case;

        return instance;
    end Make_GenMatrix;


    ----------------------------------------------------------------------------


    function Op_Is_Equal(
        left, right : in     Vkm_GenMatrix) return Vkm_Bool is

        is_equal : Vkm_Bool := true;

    begin
        for col_index in Vkm_Indices'First .. left.last_column_index loop
            for row_index in Vkm_Indices'First .. left.last_row_index loop
                if left.data(col_index, row_index) /= right.data(col_index, row_index) then
                    is_equal := false;
                end if;
                exit when is_equal = false;
            end loop;
            exit when is_equal = false;
        end loop;
        return is_equal;
    end Op_Is_Equal;



    ----------------------------------------------------------------------------


    function Op_Matrix_Mult_Matrix (left, right : in     Vkm_GenMatrix) return Vkm_GenMatrix is

        result : Vkm_GenMatrix := Make_GenMatrix(cN => right.last_column_index,
                                                 rN => left.last_row_index);

        -- It is assumed that the last_column_index of the left matrix is equal
        -- to the last_row_index of the right matrix.
        last_dot_index : constant Vkm_Indices := left.last_column_index;
    begin
        for col_index in Vkm_Indices'First .. result.last_column_index loop
            for row_index in Vkm_Indices'First .. result.last_row_index loop

                -- Perform Dot Product of left.rI with right.cI
                for dot_index in Vkm_Indices'First ..last_dot_index loop
                    result.data(col_index, row_index) := result.data(col_index, row_index) +
                        (left.data(dot_index, row_index) * right.data(col_index, dot_index));
                end loop;
            end loop;
        end loop;
        return result;
    end Op_Matrix_Mult_Matrix;


    ----------------------------------------------------------------------------


    function Apply_Func_IM_RM (im1 : in     Vkm_GenMatrix) return Vkm_GenMatrix is

        result : Vkm_GenMatrix(last_column_index => im1.last_column_index,
                               last_row_index    => im1.last_row_index);

    begin
        for col_index in Vkm_Indices'First .. result.last_column_index loop
            for row_index in Vkm_Indices'First .. result.last_row_index loop
                result.data(col_index, row_index) := Func(im1.data(col_index, row_index));
            end loop;
        end loop;
        return result;
    end Apply_Func_IM_RM;


    ----------------------------------------------------------------------------


    function Apply_Func_IM_IM_RM (im1, im2 : in     Vkm_GenMatrix) return Vkm_GenMatrix is

        result : Vkm_GenMatrix(
            last_column_index => im1.last_column_index,
            last_row_index    => im1.last_row_index);

    begin
        for col_index in Vkm_Indices'First .. result.last_column_index loop
            for row_index in Vkm_Indices'First .. result.last_row_index loop
                result.data(col_index, row_index)
                    := Func(im1.data(col_index, row_index),
                            im2.data(col_index, row_index));
            end loop;
        end loop;
        return result;
    end Apply_Func_IM_IM_RM;


    ----------------------------------------------------------------------------


    function Apply_Func_IM_IS_RM (
        im1 : in     Vkm_GenMatrix;
        is1 : in     Base_Type    ) return Vkm_GenMatrix is

        result : Vkm_GenMatrix(last_column_index => im1.last_column_index,
                               last_row_index    => im1.last_row_index);

    begin
        for col_index in Vkm_Indices'First .. result.last_column_index loop
            for row_index in Vkm_Indices'First .. result.last_row_index loop
                result.data(col_index, row_index)
                    := Func(im1.data(col_index, row_index), is1);
            end loop;
        end loop;
        return result;
    end Apply_Func_IM_IS_RM;


    ----------------------------------------------------------------------------


    function Apply_Func_IS_IM_RM (
        is1 : in     Base_Type;
        im1 : in     Vkm_GenMatrix) return Vkm_GenMatrix is

        result : Vkm_GenMatrix(last_column_index => im1.last_column_index,
                               last_row_index    => im1.last_row_index);

    begin
        for col_index in Vkm_Indices'First .. result.last_column_index loop
            for row_index in Vkm_Indices'First .. result.last_row_index loop
                result.data(col_index, row_index)
                    := Func(is1, im1.data(col_index, row_index));
            end loop;
        end loop;
        return result;
    end Apply_Func_IS_IM_RM;



end Vulkan.Math.GenMatrix;
