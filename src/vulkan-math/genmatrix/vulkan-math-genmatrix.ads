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
--< @group Vulkan Math GenMatrix
--------------------------------------------------------------------------------
--< @summary
--< This generic package provides constructors, getters, and setters for generic
--< matrix types.
--<
--< @description
--< The Vkm_Matrix type is a generic floating point matrix that can contain up to
--< 4 rows and 4 columns.
--------------------------------------------------------------------------------
generic
    type Base_Type is digits <>;
    type Base_Vector_Type is tagged private;
    with function x (vec : in     Base_Vector_Type) return Base_Type;
    with function y (vec : in     Base_Vector_Type) return Base_Type;
    with function z (vec : in     Base_Vector_Type) return Base_Type;
    with function w (vec : in     Base_Vector_Type) return Base_Type;
    with function Make_GenType (
        size                           : in Vkm_Length;
        value1, value2, value3, value4 : in Base_Type := 0.0) return Base_Vector_Type;
        
package Vulkan.Math.GenMatrix is
    pragma Preelaborate;
    pragma Pure;


    ----------------------------------------------------------------------------
    -- Types
    ----------------------------------------------------------------------------
    --< The matrix type is a 2D array with indices in the range of the Vkm_Indices
    --< type [0 .. 3]. Because of this, the number of columns is 1-4 and the 
    --< number of rows is 1-4.
    type Vkm_Matrix is array(Vkm_Indices range <>, Vkm_Indices range <>) of aliased Base_Type;
    pragma Convention(C,Vkm_Matrix);


    --< The matrix is a discriminant tagged record which encapsulates the 
    --< Vkm_Matrix type. This allows use of "." to perform functions on an 
    --< instance of matrix. 
    --< 
    --< @field last_column_index
    --< The discriminant, last_column_index, determines the number of columns in the
    --< matrix.
    --<
    --< @field last_row_index
    --< The discriminant, last_row_index, determines the number of rows in the matrix.
    --<
    --< @field data
    --< The matrix data for the record. This information is able to be
    --< passed to a C/C++ context.
    type Vkm_GenMatrix(last_column_index : Vkm_Indices;
                       last_row_index    : Vkm_Indices) is tagged
        record
            data : Vkm_Matrix(Vkm_Indices'First .. last_column_index,
                              Vkm_Indices'First .. last_row_index);
        end record;


   --< A reference to a generic matrix type. The Vkm_GenMatrix instance is 
   --< automatically dereferenced on use.
    type Vkm_GenMatrix_Reference(instance : not null access Vkm_GenMatrix) is null record
        with Implicit_Dereference => instance;
        
        
    ----------------------------------------------------------------------------
    -- Element Getters
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenMatrix element accessor.
    --<
    --< @description
    --< Gets the value of an element at the specified column and row index.
    --<
    --< @param instance
    --< The Vkm_GenMatrix instance to get the element value of.
    --<
    --< @param col_index
    --< The index of the column at which to get an element.
    --<
    --< @param row_index
    --< The index of the row at which to get an element.
    --<
    --< @return
    --< The value of the specified element.
    ----------------------------------------------------------------------------
    function Element(instance             : in     Vkm_GenMatrix;
                     col_index, row_index : in     Vkm_Indices) return Base_Type;

    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c0r0 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 0, 0)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c0r1 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 0, 1)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c0r2 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 0, 2)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c0r3 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 0, 3)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c1r0 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 1, 0)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c1r1 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 1, 1)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c1r2 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 1, 2)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c1r3 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 1, 3)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c2r0 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 2, 0)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c2r1 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 2, 1)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c2r2 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 2, 2)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c2r3 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 2, 3)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c3r0 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 3, 0)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c3r1 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 3, 1)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c3r2 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 3, 2)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c3r3 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 3, 3)) with Inline;


    ----------------------------------------------------------------------------
    -- Element Setters
    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenMatrix element accessor.
    --<
    --< @description
    --< Sets the element at the specified column and row index to a value.
    --<
    --< @param instance
    --< The Vkm_GenMatrix instance to set the element of.
    --<
    --< @param col_index
    --< The index of the column at which to set an element.
    --<
    --< @param row_index
    --< The index of the row at which to set an element.
    --<
    --< @param value
    --< The value to set the specified element.
    ----------------------------------------------------------------------------
    procedure Element(
        instance             : in out Vkm_GenMatrix;
        col_index, row_index : in     Vkm_Indices;
        value                : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c0r0( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c0r1( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c0r2( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c0r3( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c1r0( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c1r1( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c1r2( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c1r3( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c2r0( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c2r1( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c2r2( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c2r3( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c3r0( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c3r1( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c3r2( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    procedure c3r3( 
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    ----------------------------------------------------------------------------
    --< @summary
    --< Vkm_GenMatrix element accessor.
    --<
    --< @description
    --< Sets the element at the specified column and row index to a value. A 
    --< reference to the matrix is returned upon completion.
    --<
    --< @param instance
    --< The Vkm_GenMatrix instance to set the element of.
    --<
    --< @param col_index
    --< The index of the column at which to set an element.
    --<
    --< @param row_index
    --< The index of the row at which to set an element.
    --<
    --< @param value
    --< The value to set the specified element.
    --<
    --< @return
    --< A reference to the Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function  Element(
        instance             : in out Vkm_GenMatrix;
        col_index, row_index : in     Vkm_Indices  ;
        value                : in     Base_Type    ) return Vkm_GenMatrix_Reference;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c0r0 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 0, 0, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c0r1 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 0, 1, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c0r2 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 0, 2, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c0r3 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 0, 3, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c1r0 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 1, 0, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c1r1 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 1, 1, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c1r2 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 1, 2, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c1r3 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 1, 3, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c2r0 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 2, 0, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c2r1 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 2, 1, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c2r2 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 2, 2, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c2r3 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 2, 3, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c3r0 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 3, 0, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c3r1 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 3, 1, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c3r2 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 3, 2, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    function c3r3 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 3, 3, value)) with Inline;


    ----------------------------------------------------------------------------
    -- Column Accessors
    ----------------------------------------------------------------------------
    --< @summary
    --< Get the indicated column of the matrix as a vector.
    --<
    --< @description
    --< Retrieve the indicated column of the matrix as a vector:
    --<
    --<    cI := [ instance.cIr0 instance.cIr1 ... instance.cIrN ]
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which the column is retrieved.
    --<
    --< @param col_index
    --< The index of the column to retrieve from the matrix.
    --<
    --< @return
    --< The vector that contains all elements in the indicated column.
    ----------------------------------------------------------------------------
    function Column (
        instance  : in     Vkm_GenMatrix;
        col_index : in     Vkm_Indices) return Base_Vector_Type is
        (Make_GenType(
             To_Vkm_Length(instance.last_row_index),
             instance.Element(col_index, 0),
             instance.Element(col_index, 1),
             instance.Element(col_index, 2),
             instance.Element(col_index, 3))) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    function c0 (
        instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Column(instance, 0)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    function c1 (
        instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Column(instance, 1)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    function c2 (
        instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Column(instance, 2)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    function c3 (
        instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Column(instance, 3)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Set the indicated column of the matrix given a vector.
    --<
    --< @description
    --< Sets the indicated column of the matrix to the specified value.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which the column is set.
    --<
    --< @param col_index
    --< The index of the column to set for the matrix.
    --<
    --< @param col_val
    --< The vector value to set the column to.
    ----------------------------------------------------------------------------
    procedure Column (
        instance  : in out Vkm_GenMatrix;
        col_index : in     Vkm_Indices;
        col_val   : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    procedure c0 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    procedure c1 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    procedure c2 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    procedure c3 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type);


    ----------------------------------------------------------------------------
    --< @summary
    --< Set the indicated column of the matrix given a vector.
    --<
    --< @description
    --< Sets the indicated column of the matrix to the specified value.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which the column is set.
    --<
    --< @param col_index
    --< The index of the column to set for the matrix.
    --<
    --< @param col_val
    --< The vector value to set the column to.
    --<
    --< @return 
    --< A reference to the Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function  Column(
        instance  : in out Vkm_GenMatrix;
        col_index : in     Vkm_Indices  ;
        col_val   : in     Base_Vector_Type) return Vkm_GenMatrix_Reference;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    function c0 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Column(instance, 0, col_val)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    function c1 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Column(instance, 1, col_val)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    function c2 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Column(instance, 2, col_val)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    function c3 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Column(instance, 3, col_val)) with Inline;


    ----------------------------------------------------------------------------
    -- Row Accessors
    ----------------------------------------------------------------------------
    --< @summary
    --< Get the indicated row of the matrix as a vector.
    --<
    --< @description
    --< Retrieve the indicated row of the matrix as a vector:
    --<
    --<    rI := [ instance.c0rI instance.c1rI ... instance.cNrI ]
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which the row is retrieved.
    --<
    --< @param row_index
    --< The index of the row to retrieve from the matrix.
    --<
    --< @return
    --< The vector that contains all elements in the indicated row.
    ----------------------------------------------------------------------------
    function Row (
        instance  : in     Vkm_GenMatrix;
        row_index : in     Vkm_Indices) return Base_Vector_Type is
        (Make_GenType(
             To_Vkm_Length(instance.last_row_index),
             instance.Element(0, row_index),
             instance.Element(1, row_index),
             instance.Element(2, row_index),
             instance.Element(3, row_index))) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    function r0 (instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Row(instance, 0)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    function r1 (instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Row(instance, 1)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    function r2 (instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Row(instance, 2)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    function r3 (instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Row(instance, 3)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Set the indicated row of the matrix given a vector.
    --<
    --< @description
    --< Sets the indicated row of the matrix to the specified value.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which the row is set.
    --<
    --< @param row_index
    --< The index of the row to set for the matrix.
    --<
    --< @param row_val
    --< The vector value to set the row to.
    ----------------------------------------------------------------------------
    procedure Row (
        instance  : in out Vkm_GenMatrix;
        row_index : in     Vkm_Indices;
        row_val   : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    procedure r0 (
        instance : in out Vkm_GenMatrix;
        row_val      : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    procedure r1 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    procedure r2 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    procedure r3 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type);


    ----------------------------------------------------------------------------
    --< @summary
    --< Set the indicated row of the matrix given a vector.
    --<
    --< @description
    --< Sets the indicated row of the matrix to the specified value.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which the row is set.
    --<
    --< @param row_index
    --< The index of the row to set for the matrix.
    --<
    --< @param row_val
    --< The vector value to set the row to.
    --<
    --< @return
    --< A reference to the Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Row (
        instance  : in out Vkm_GenMatrix;
        row_index : in     Vkm_Indices;
        row_val   : in     Base_Vector_Type) return Vkm_GenMatrix_Reference;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    function r0 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Row(instance, 0, row_val)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    function r1 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Row(instance, 1, row_val)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    function r2 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Row(instance, 2, row_val)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    function r3 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Row(instance, 3, row_val)) with Inline;


    ----------------------------------------------------------------------------
    -- Operations
    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_GenMatrix type.
    --<
    --< @description
    --< Creates a new instance of Vkm_GenMatrix with the indicated number of rows
    --< and columns. Each element is initialized as specified:
    --< 
    --<    \      c0        c1       c2       c3      cN 
    --<    r0 | c0r0_val c1r0_val c2r0_val c3r0_val |
    --<    r1 | c0r1_val c1r1_val c2r1_val c3r1_val |
    --<    r2 | c0r2_val c1r2_val c2r2_val c3r2_val |
    --<    r3 | c0r3_val c1r3_val c2r3_val c3r3_val |
    --<    rN
    --<
    --< If no value is supplied for an element a default value of 0.0 is used. 
    --< If the indices in the element name are not within matrix bounds, value 
    --< specifiedis ignored.
    --<
    --< @param cN
    --< The last index that can be used for accessing columns in the matrix.
    --<
    --< @param rN
    --< The last index that can be used for accessing rows in the matrix.
    --<
    --< @param c0r0_val
    --< The value to set for the element at column 0 and row 0.
    --<
    --< @param c0r1_val
    --< The value to set for the element at column 0 and row 1.
    --<
    --< @param c0r2_val
    --< The value to set for the element at column 0 and row 2.
    --<
    --< @param c0r3_val
    --< The value to set for the element at column 0 and row 3.
    --<
    --< @param c1r0_val
    --< The value to set for the element at column 1 and row 0.
    --<
    --< @param c1r1_val
    --< The value to set for the element at column 1 and row 1.
    --<
    --< @param c1r2_val
    --< The value to set for the element at column 1 and row 2.
    --<
    --< @param c1r3_val
    --< The value to set for the element at column 1 and row 3.
    --<
    --< @param c2r0_val
    --< The value to set for the element at column 2 and row 0.
    --<
    --< @param c2r1_val
    --< The value to set for the element at column 2 and row 1.
    --<
    --< @param c2r2_val
    --< The value to set for the element at column 2 and row 2.
    --<
    --< @param c2r3_val
    --< The value to set for the element at column 2 and row 3.
    --<
    --< @param c3r0_val
    --< The value to set for the element at column 3 and row 0. 
    --<
    --< @param c3r1_val
    --< The value to set for the element at column 3 and row 1.
    --<
    --< @param c3r2_val
    --< The value to set for the element at column 3 and row 2.
    --<
    --< @param c3r3_val
    --< The value to set for the element at column 3 and row 3.
    --<
    --< @return
    --< The new Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Make_GenMatrix(
        cN, rN                                 : in     Vkm_Indices;
        c0r0_val, c0r1_val, c0r2_val, c0r3_val,
        c1r0_val, c1r1_val, c1r2_val, c1r3_val,
        c2r0_val, c2r1_val, c2r2_val, c2r3_val,
        c3r0_val, c3r1_val, c3r2_val, c3r3_val : in     Base_Type := 0.0) return Vkm_GenMatrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_GenMatrix type.
    --<
    --< @description
    --< Creates a new instance of Vkm_GenMatrix with the indicated number of rows
    --< and columns. Each element is initialized as specified:
    --< 
    --<    \      c0        c1       c2       c3      cN 
    --<    r0 |  diag      0.0      0.0      0.0    |
    --<    r1 |   0.0     diag      0.0      0.0    |
    --<    r2 |   0.0      0.0     diag      0.0    |
    --<    r3 |   0.0      0.0      0.0     diag    |
    --<    rN
    --<
    --< @param cN
    --< The last index that can be used for accessing columns in the matrix.
    --<
    --< @param rN
    --< The last index that can be used for accessing rows in the matrix.
    --<
    --< @param diag
    --< The value to set on the diagonal. If no value is supplied, this value's
    --< default value is 1.0, and the new matrix is initialized with values of
    --< the identity matrix.
    --<
    --< @return
    --< The new Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Make_GenMatrix(
        cN, rN : in     Vkm_Indices      ;
        diag   : in     Base_Type  := 1.0) return Vkm_GenMatrix is
        (Make_GenMatrix(
             cN => cN, rN => rN,
             c0r0_val => diag,
             c1r1_val => diag,
             c2r2_val => diag,
             c3r3_val => diag)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_GenMatrix type.
    --<
    --< @description
    --< Creates a new instance of Vkm_GenMatrix with the indicated number of rows
    --< and columns. Each element is initialized as specified:
    --< 
    --<    \      c0       c1       c2       c3      cN 
    --<    r0 | sub.c0r0 sub.c1r0 sub.c2r0 sub.c3r0 |
    --<    r1 | sub.c0r1 sub.c1r1 sub.c2r1 sub.c3r1 |
    --<    r2 | sub.c0r2 sub.c1r2 sub.c2r2 sub.c3r2 |
    --<    r3 | sub.c0r3 sub.c1r3 sub.c2r3 sub.c3r3 |
    --<    rN
    --<
    --<
    --< @param cN
    --< The last index that can be used for accessing columns in the matrix.
    --<
    --< @param rN
    --< The last index that can be used for accessing rows in the matrix.
    --<
    --< @param sub
    --< The submatrix used to initialize elements of the new instance of matrix.
    --< If an element is out of bounds for the submatrix, the corresponding value
    --< of the identity matrix is used instead.
    --<
    --< @return
    --< The new Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Make_GenMatrix(
        cN, rN : in     Vkm_Indices;
        sub    : in     Vkm_GenMatrix) return Vkm_GenMatrix is
        (Make_GenMatrix(
             cN => cN, rN => rN,
             c0r0_val => sub.c0r0, c0r1_val => sub.c0r1, c0r2_val => sub.c0r2, c0r3_val => sub.c0r3,
             c1r0_val => sub.c1r0, c1r1_val => sub.c1r1, c1r2_val => sub.c1r2, c1r3_val => sub.c1r3,
             c2r0_val => sub.c2r0, c2r1_val => sub.c2r1, c2r2_val => sub.c2r2, c2r3_val => sub.c2r3,
             c3r0_val => sub.c3r0, c3r1_val => sub.c3r1, c3r2_val => sub.c3r2, c3r3_val => sub.c3r3)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_GenMatrix type.
    --<
    --< @description
    --< Creates a new instance of Vkm_GenMatrix with the indicated number of rows
    --< and columns. Each element is initialized as specified:
    --< 
    --<    \      c0        c1       c2       c3      cN 
    --<    r0 |  diag.x    0.0      0.0      0.0    |
    --<    r1 |   0.0     diag.y    0.0      0.0    |
    --<    r2 |   0.0      0.0     diag.z    0.0    |
    --<    r3 |   0.0      0.0      0.0     diag.w  |
    --<    rN
    --<
    --< @param cN
    --< The last index that can be used for accessing columns in the matrix.
    --<
    --< @param rN
    --< The last index that can be used for accessing rows in the matrix.
    --<
    --< @param diag
    --< The value to set on the diagonal.
    --<
    --< @return
    --< The new Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Make_GenMatrix(
        cN, rN : in     Vkm_Indices;
        diag   : in     Base_Vector_Type) return Vkm_GenMatrix is
        (Make_GenMatrix(
            cN => cN, rN => rN,
            c0r0_val => x(diag),
            c1r1_val => y(diag),
            c2r2_val => z(diag),
            c3r3_val => w(diag))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_GenMatrix type.
    --<
    --< @description
    --< Creates a new instance of Vkm_GenMatrix with the indicated number of rows
    --< and columns. Each element is initialized as specified:
    --< 
    --<    \      c0       c1       c2       c3      cN 
    --<    r0 | c0_val.x c1_val.x   0.0      0.0    |
    --<    r1 | c0_val.y c1_val.y   0.0      0.0    |
    --<    r2 | c0_val.z c1_val.z   0.0      0.0    |
    --<    r3 | c0_val.w c1_val.w   0.0      0.0    |
    --<    rN
    --<
    --< If an index is out of bounds for a vector, the default value of 0.0 is 
    --< used for the corresponding matrix element.
    --<
    --< @param cN
    --< The last index that can be used for accessing columns in the matrix.
    --<
    --< @param rN
    --< The last index that can be used for accessing rows in the matrix.
    --<
    --< @param c0_val
    --< The value used to initialize column 0 of the matrix.
    --<
    --< @param c1_val
    --< The value used to initialize column 1 of the matrix.
    --<
    --< @return
    --< The new Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Make_GenMatrix(
        cN, rN         : in     Vkm_Indices;
        c0_val, c1_val : in     Base_Vector_Type) return Vkm_GenMatrix is
        (Make_GenMatrix(
            cN => cN, rN => rN,
            c0r0_val => x(c0_val), c0r1_val => y(c0_val), c0r2_val => z(c0_val), c0r3_val => w(c0_val),
            c1r0_val => x(c1_val), c1r1_val => y(c1_val), c1r2_val => z(c1_val), c1r3_val => w(c1_val))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_GenMatrix type.
    --<
    --< @description
    --< Creates a new instance of Vkm_GenMatrix with the indicated number of rows
    --< and columns. Each element is initialized as specified:
    --< 
    --<    \      c0       c1       c2       c3      cN 
    --<    r0 | c0_val.x c1_val.x c2_val.x   0.0    |
    --<    r1 | c0_val.y c1_val.y c2_val.y   0.0    |
    --<    r2 | c0_val.z c1_val.z c2_val.z   0.0    |
    --<    r3 | c0_val.w c1_val.w c2_val.w   0.0    |
    --<    rN
    --<
    --< If an index is out of bounds for a vector, the default value of 0.0 is 
    --< used for the corresponding matrix element.
    --<
    --< @param cN
    --< The last index that can be used for accessing columns in the matrix.
    --<
    --< @param rN
    --< The last index that can be used for accessing rows in the matrix.
    --<
    --< @param c0_val
    --< The value used to initialize column 0 of the matrix.
    --<
    --< @param c1_val
    --< The value used to initialize column 1 of the matrix.
    --<
    --< @param c2_val
    --< The value used to initialize column 2 of the matrix.
    --<
    --< @return
    --< The new Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Make_GenMatrix(
        cN, rN                 : in     Vkm_Indices;
        c0_val, c1_val, c2_val : in     Base_Vector_Type) return Vkm_GenMatrix is
        (Make_GenMatrix(
            cN => cN, rN => rN,
            c0r0_val => x(c0_val), c0r1_val => y(c0_val), c0r2_val => z(c0_val), c0r3_val => w(c0_val),
            c1r0_val => x(c1_val), c1r1_val => y(c1_val), c1r2_val => z(c1_val), c1r3_val => w(c1_val),
            c2r0_val => x(c2_val), c2r1_val => y(c2_val), c2r2_val => z(c2_val), c2r3_val => w(c2_val))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor for Vkm_GenMatrix type.
    --<
    --< @description
    --< Creates a new instance of Vkm_GenMatrix with the indicated number of rows
    --< and columns. Each element is initialized as specified:
    --< 
    --<    \      c0       c1       c2       c3      cN 
    --<    r0 | c0_val.x c1_val.x c2_val.x c3_val.x |
    --<    r1 | c0_val.y c1_val.y c2_val.y c3_val.y |
    --<    r2 | c0_val.z c1_val.z c2_val.z c3_val.z |
    --<    r3 | c0_val.w c1_val.w c2_val.w c3_val.w |
    --<    rN
    --<
    --< If an index is out of bounds for a vector, the default value of 0.0 is 
    --< used for the corresponding matrix element.
    --<
    --< @param cN
    --< The last index that can be used for accessing columns in the matrix.
    --<
    --< @param rN
    --< The last index that can be used for accessing rows in the matrix.
    --<
    --< @param c0_val
    --< The value used to initialize column 0 of the matrix.
    --<
    --< @param c1_val
    --< The value used to initialize column 1 of the matrix.
    --<
    --< @param c2_val
    --< The value used to initialize column 2 of the matrix.
    --<
    --< @return
    --< The new Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Make_GenMatrix(
        cN, rN                         : in     Vkm_Indices;
        c0_val, c1_val, c2_val, c3_val : in     Base_Vector_Type) return Vkm_GenMatrix is
        (Make_GenMatrix(
            cN => cN, rN => rN,
            c0r0_val => x(c0_val), c0r1_val => y(c0_val), c0r2_val => z(c0_val), c0r3_val => w(c0_val),
            c1r0_val => x(c1_val), c1r1_val => y(c1_val), c1r2_val => z(c1_val), c1r3_val => w(c1_val),
            c2r0_val => x(c2_val), c2r1_val => y(c2_val), c2r2_val => z(c2_val), c2r3_val => w(c2_val),
            c3r0_val => x(c3_val), c3r1_val => y(c3_val), c3r2_val => z(c3_val), c3r3_val => w(c3_val))) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function component-wise on two matrices.
    --<
    --< @description
    --< Applies function component-wise on two matrices, yielding the following
    --< matrix:
    --<
    --<     | Func(im1.c0r0, im2.c0r0) ... Func(im1.cNr0, im2.cNr0) |
    --<     |        ...                           ...              |
    --<     | Func(im1.c0rN, im2.c0rN) ... Func(im1.cNrN, im2.cNrN) |
    --<
    --< @param im1
    --< The first input Vkm_GenMatrix parameter.
    --<
    --< @param im2
    --< The second input Vkm_GenMatrix parameter.
    --<
    --< @return
    --< The result from applying the generic function Func component-wise on both
    --< matrices.
    ----------------------------------------------------------------------------
    generic
        with function Func (is1, is2 : in     Base_Type) return Base_Type;
    function Apply_Func_IM_IM_RM (im1, im2 : in     Vkm_GenMatrix) return Vkm_GenMatrix;
    
    
end Vulkan.Math.GenMatrix;
