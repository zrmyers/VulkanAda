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
    
    
    function Make_GenMatrix(
        cN, rN                                 : in     Vkm_Indices;
        c0r0_val, c0r1_val, c0r2_val, c0r3_val,
        c1r0_val, c1r1_val, c1r2_val, c1r3_val,
        c2r0_val, c2r1_val, c2r2_val, c2r3_val,
        c3r0_val, c3r1_val, c3r2_val, c3r3_val : in     Base_Type := 0.0) return Vkm_GenMatrix is
        
        instance : Vkm_GenMatrix(last_column_index => cN, last_row_index => rN);
    begin
        if cN = 0 and rN = 0 then
            instance.data := (0 => (0 => c0r0_val));
        elsif cN = 0 and rN = 1 then
            instance.data := (0 => (c0r0_val, c0r1_val));
        elsif cN = 0 and rN = 2 then
            instance.data := (0 => (c0r0_val, c0r1_val, c0r2_val));
        elsif cN = 0 and rN = 3 then
            instance.data := (0 => (c0r0_val, c0r1_val, c0r2_val, c0r3_val));
        elsif cN = 1 and rN = 0 then
            instance.data := ((0 => c0r0_val),
                              (0 => c1r0_val));
        elsif cN = 1 and rN = 1 then
            instance.data := ((c0r0_val, c0r1_val),
                              (c1r0_val, c1r1_val));
        elsif cN = 1 and rN = 2 then
            instance.data := ((c0r0_val, c0r1_val, c0r2_val),
                              (c1r0_val, c1r1_val, c1r2_val));
        elsif cN = 1 and rN = 3 then
            instance.data := ((c0r0_val, c0r1_val, c0r2_val, c0r3_val),
                              (c1r0_val, c1r1_val, c1r2_val, c1r3_val));
        elsif cN = 2 and rN = 0 then
            instance.data := ((0 => c0r0_val),
                              (0 => c1r0_val),
                              (0 => c2r0_val));
        elsif cN = 2 and rN = 1 then 
            instance.data := ((c0r0_val, c0r1_val),
                              (c1r0_val, c1r1_val),
                              (c2r0_val, c2r1_val));
        elsif cN = 2 and rN = 2 then
            instance.data := ((c0r0_val, c0r1_val, c0r2_val),
                              (c1r0_val, c1r1_val, c1r2_val),
                              (c2r0_val, c2r1_val, c2r2_val));
        elsif cN = 2 and rN = 3 then
            instance.data := ((c0r0_val, c0r1_val, c0r2_val, c0r3_val),
                              (c1r0_val, c1r1_val, c1r2_val, c1r3_val),
                              (c2r0_val, c2r1_val, c2r2_val, c2r3_val));
        elsif cN = 3 and rN = 0 then
            instance.data := ((0 => c0r0_val),
                              (0 => c1r0_val),
                              (0 => c2r0_val),
                              (0 => c2r0_val));
        elsif cN = 3 and rN = 1 then 
            instance.data := ((c0r0_val, c0r1_val),
                              (c1r0_val, c1r1_val),
                              (c2r0_val, c2r1_val),
                              (c3r0_val, c3r1_val));
        elsif cN = 3 and rN = 2 then
            instance.data := ((c0r0_val, c0r1_val, c0r2_val),
                              (c1r0_val, c1r1_val, c1r2_val),
                              (c2r0_val, c2r1_val, c2r2_val),
                              (c3r0_val, c3r1_val, c3r2_val));
        elsif cN = 3 and rN = 3 then
            instance.data := ((c0r0_val, c0r1_val, c0r2_val, c0r3_val),
                              (c1r0_val, c1r1_val, c1r2_val, c1r3_val),
                              (c2r0_val, c2r1_val, c2r2_val, c2r3_val),
                              (c3r0_val, c3r1_val, c3r2_val, c3r3_val));
        end if;
        return instance;
    end Make_GenMatrix;


    ----------------------------------------------------------------------------
    
    
end Vulkan.Math.GenMatrix;
