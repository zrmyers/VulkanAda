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
    type Base_Vector_Type (<>) is tagged private;

    with function Image (instance : Base_Vector_Type) return String;

    ----------------------------------------------------------------------------
    --< @summary
    --< Retrieve the x-component of the vector.
    --<
    --< @description
    --< Retrieve the x-component of the vector.
    --<
    --< @param vec
    --< The vector to retrieve the x-component from.
    --<
    --< @return
    --< The x-component of the vector.
    ----------------------------------------------------------------------------
    with function x (vec : in     Base_Vector_Type) return Base_Type;


    ----------------------------------------------------------------------------
    --< @summary
    --< Retrieve the y-component of the vector.
    --<
    --< @description
    --< Retrieve the y-component of the vector.
    --<
    --< @param vec
    --< The vector to retrieve the y-component from.
    --<
    --< @return
    --< The y-component of the vector.
    ----------------------------------------------------------------------------
    with function y (vec : in     Base_Vector_Type) return Base_Type;


    ----------------------------------------------------------------------------
    --< @summary
    --< Retrieve the z-component of the vector.
    --<
    --< @description
    --< Retrieve the z-component of the vector.
    --<
    --< @param vec
    --< The vector to retrieve the z-component from.
    --<
    --< @return
    --< The z-component of the vector.
    ----------------------------------------------------------------------------
    with function z (vec : in     Base_Vector_Type) return Base_Type;


    ----------------------------------------------------------------------------
    --< @summary
    --< Retrieve the w-component of the vector.
    --<
    --< @description
    --< Retrieve the w-component of the vector.
    --<
    --< @param vec
    --< The vector to retrieve the w-component from.
    --<
    --< @return
    --< The w-component of the vector.
    ----------------------------------------------------------------------------
    with function w (vec : in     Base_Vector_Type) return Base_Type;


    ----------------------------------------------------------------------------
    --< @summary
    --< Construct a Base_Vector_Type.
    --<
    --< @description
    --< Construct a Base_Vector_Type.
    --<
    --< @param size
    --< The number of components in the Base_Vector_Type.
    --<
    --< @param value1
    --< The value to set for the x-component of the Base_Vector_Type.
    --<
    --< @param value2
    --< The value to set for the y-component of the Base_Vector_Type.
    --<
    --< @param value3
    --< The value to set for the z-component of the Base_Vector_Type.
    --<
    --< @param value4
    --< The value to set for the w-component of the Base_Vector_Type.
    --<
    --< @return
    --< The w-component of the vector.
    ----------------------------------------------------------------------------
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
    --< @summary
    --< The image of the matrix
    --<
    --< @description
    --< Generates a human readable string which contains the contents of the
    --< instance of Vkm_GenMatrix. For a 2x2 matrix, this appears as a list of
    --< the row vectors
    --<
    --<     "[ " & Image(mat.r0) & ", " & Image(mat.r1) & " ]"
    --<
    --< The Image() subprogram that is supplied during generic instantiation is used
    --< to print the component of Base_Vector_Type.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix.
    --<
    --< @return
    --< The human readable image of the matrix
    ----------------------------------------------------------------------------
    function Image (instance : in     Vkm_GenMatrix) return String;

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
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c0r0 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 0, 0)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c0r1 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 0, 1)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c0r2 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 0, 2)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c0r3 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 0, 3)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c1r0 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 1, 0)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c1r1 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 1, 1)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c1r2 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 1, 2)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c1r3 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 1, 3)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c2r0 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 2, 0)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c2r1 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 2, 1)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c2r2 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 2, 2)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c2r3 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 2, 3)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c3r0 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 3, 0)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c3r1 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 3, 1)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
    function c3r2 (instance : in     Vkm_GenMatrix) return Base_Type is
        (Element(instance, 3, 2)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to retrieve the element from.
    --<
    --< @return
    --< The element from the matrix.
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
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c0r0(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c0r1(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c0r2(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c0r3(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c1r0(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c1r1(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c1r2(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c1r3(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c2r0(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c2r1(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c2r2(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c2r3(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c3r0(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c3r1(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    procedure c3r2(
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type);


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
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
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c0r0 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 0, 0, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c0r1 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 0, 1, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c0r2 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 0, 2, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c0r3 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 0, 3, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c1r0 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 1, 0, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c1r1 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 1, 1, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c1r2 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 1, 2, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c1r3 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 1, 3, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c2r0 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 2, 0, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c2r1 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 2, 1, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c2r2 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 2, 2, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c2r3 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 2, 3, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c3r0 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 3, 0, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c3r1 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 3, 1, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
    function c3r2 (
        instance : in out Vkm_GenMatrix;
        value    : in     Base_Type    ) return Vkm_GenMatrix_Reference is
        (Element(instance, 3, 2, value)) with Inline;


    --< @private
    --< This is a named accessor for an element of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The Instance of Vkm_GenMatrix to set the element for.
    --<
    --< @param value
    --< The value to set for the matrix element.
    --<
    --< @return
    --< A reference to the modified matrix instance.
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
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which to retrieve a column.
    --<
    --< @return
    --< The indicated column from the matrix.
    function c0 (
        instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Column(instance, 0)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which to retrieve a column.
    --<
    --< @return
    --< The indicated column from the matrix.
    function c1 (
        instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Column(instance, 1)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which to retrieve a column.
    --<
    --< @return
    --< The indicated column from the matrix.
    function c2 (
        instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Column(instance, 2)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which to retrieve a column.
    --<
    --< @return
    --< The indicated column from the matrix.
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
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a column.
    --<
    --< @param col_val
    --< The vector to set the column equal to.
    procedure c0 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a column.
    --<
    --< @param col_val
    --< The vector to set the column equal to.
    procedure c1 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a column.
    --<
    --< @param col_val
    --< The vector to set the column equal to.
    procedure c2 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a column.
    --<
    --< @param col_val
    --< The vector to set the column equal to.
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
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a column.
    --<
    --< @param col_val
    --< The vector to set the column equal to.
    --<
    --< @return
    --< A reference to the instance of Vkm_GenMatrix.
    function c0 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Column(instance, 0, col_val)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a column.
    --<
    --< @param col_val
    --< The vector to set the column equal to.
    --<
    --< @return
    --< A reference to the instance of Vkm_GenMatrix.
    function c1 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Column(instance, 1, col_val)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a column.
    --<
    --< @param col_val
    --< The vector to set the column equal to.
    --<
    --< @return
    --< A reference to the instance of Vkm_GenMatrix.
    function c2 (
        instance : in out Vkm_GenMatrix;
        col_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Column(instance, 2, col_val)) with Inline;


    --< @private
    --< This is a named accessor for a column of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a column.
    --<
    --< @param col_val
    --< The vector to set the column equal to.
    --<
    --< @return
    --< A reference to the instance of Vkm_GenMatrix.
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
             To_Vkm_Length(instance.last_column_index),
             instance.Element(0, row_index),
             instance.Element(1, row_index),
             instance.Element(2, row_index),
             instance.Element(3, row_index))) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which to retrieve a row.
    --<
    --< @return
    --< The indicated row from the matrix.
    function r0 (instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Row(instance, 0)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which to retrieve a row.
    --<
    --< @return
    --< The indicated row from the matrix.
    function r1 (instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Row(instance, 1)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which to retrieve a row.
    --<
    --< @return
    --< The indicated row from the matrix.
    function r2 (instance : in     Vkm_GenMatrix) return Base_Vector_Type is
        (Row(instance, 2)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix from which to retrieve a row.
    --<
    --< @return
    --< The indicated row from the matrix.
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
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a row.
    --<
    --< @param row_val
    --< The the value to set a row of the matrix to.
    procedure r0 (
        instance : in out Vkm_GenMatrix;
        row_val      : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a row.
    --<
    --< @param row_val
    --< The the value to set a row of the matrix to.
    procedure r1 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a row.
    --<
    --< @param row_val
    --< The the value to set a row of the matrix to.
    procedure r2 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type);


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a row.
    --<
    --< @param row_val
    --< The the value to set a row of the matrix to.
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
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a row.
    --<
    --< @param row_val
    --< The the value to set a row of the matrix to.
    --<
    --< @return
    --< A reference to the modified matrix.
    function r0 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Row(instance, 0, row_val)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a row.
    --<
    --< @param row_val
    --< The the value to set a row of the matrix to.
    --<
    --< @return
    --< A reference to the modified matrix.
    function r1 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Row(instance, 1, row_val)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a row.
    --<
    --< @param row_val
    --< The the value to set a row of the matrix to.
    --<
    --< @return
    --< A reference to the modified matrix.
    function r2 (
        instance : in out Vkm_GenMatrix;
        row_val  : in     Base_Vector_Type) return Vkm_GenMatrix_Reference is
        (Row(instance, 2, row_val)) with Inline;


    --< @private
    --< This is a named accessor for a row of an instance of Vkm_GenMatrix.
    --<
    --< @param instance
    --< The instance of Vkm_GenMatrix for which to set a row.
    --<
    --< @param row_val
    --< The the value to set a row of the matrix to.
    --<
    --< @return
    --< A reference to the modified matrix.
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
    --< The value to set on the diagonal.
    --<
    --< @return
    --< The new Vkm_GenMatrix instance.
    ----------------------------------------------------------------------------
    function Make_GenMatrix(
        cN, rN : in     Vkm_Indices;
        diag   : in     Base_Type) return Vkm_GenMatrix is
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
    --< Determine whether two matrices are equal to each other.
    --<
    --< @description
    --< Determines whether every element of the two matrices are equal to each
    --< other.
    --<
    --< @param left
    --< The variable that is to the left of the equality operator.
    --<
    --< @param right
    --< The variable that is to the right of the equality operator.
    --<
    --< @return
    --< True if the matrices are equal to each other. Otherwise, false.
    ----------------------------------------------------------------------------
    function Op_Is_Equal(
        left, right : in     Vkm_GenMatrix) return Vkm_Bool;


    ----------------------------------------------------------------------------
    --< @summary
    --< Linear algebraic matrix multiplication
    --<
    --< @description
    --< Perform linear algebraic matrix multiplication for the two matrices.
    --<
    --< @param left
    --< The left matrix in the computation.
    --<
    --< @param right
    --< The right matrix in the computation.
    --<
    --< The result of linear algebraic multiplication for the two matrices.
    ----------------------------------------------------------------------------
    function Op_Matrix_Mult_Matrix (
        left, right : in     Vkm_GenMatrix) return Vkm_GenMatrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function component-wise on a matrix
    --<
    --< @description
    --< Applies function component-wise on a matrix, yielding the following
    --< matrix:
    --<
    --<     | Func(im1.c0r0) ... Func(im1.cNr0) |
    --<     |        ...           ...          |
    --<     | Func(im1.c0rN) ... Func(im1.cNrN) |
    --<
    --< @param im1
    --< The input Vkm_GenMatrix parameter.
    --<
    --< @return
    --< The result from applying the generic function Func component-wise on a
    --< matrix.
    ----------------------------------------------------------------------------
    generic
        with function Func (is1 : in     Base_Type) return Base_Type;
    function Apply_Func_IM_RM (im1 : in     Vkm_GenMatrix) return Vkm_GenMatrix;


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


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function component-wise on a matrix and a scalar.
    --<
    --< @description
    --< Applies function component-wise on a matrix and a scalar, yielding the
    --< following matrix:
    --<
    --<     | Func(im1.c0r0, is1) ... Func(im1.cNr0, is1) |
    --<     |        ...                    ...           |
    --<     | Func(im1.c0rN, is1) ... Func(im1.cNrN, is1) |
    --<
    --< @param im1
    --< The first input Vkm_GenMatrix parameter.
    --<
    --< @param is1
    --< The second input Vkm_GenMatrix parameter.
    --<
    --< @return
    --< The result from applying the generic function Func component-wise on both
    --< matrices.
    ----------------------------------------------------------------------------
    generic
        with function Func (is1, is2 : in     Base_Type) return Base_Type;
    function Apply_Func_IM_IS_RM (
        im1 : in     Vkm_GenMatrix;
        is1 : in     Base_Type    ) return Vkm_GenMatrix;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function component-wise on a matrix and a scalar.
    --<
    --< @description
    --< Applies function component-wise on a matrix and a scalar, yielding the
    --< following matrix:
    --<
    --<     | Func(is1, im1.c0r0) ... Func(is1, im1.cNr0) |
    --<     |        ...                     ...          |
    --<     | Func(is1, im1.c0rN) ... Func(is1, im1.cNrN) |
    --<
    --< @param is1
    --< The first input Vkm_GenMatrix parameter.
    --<
    --< @param im1
    --< The second input Vkm_GenMatrix parameter.
    --<
    --< @return
    --< The result from applying the generic function Func component-wise on both
    --< matrices.
    ----------------------------------------------------------------------------
    generic
        with function Func (is1, is2 : in     Base_Type) return Base_Type;
    function Apply_Func_IS_IM_RM (
        is1 : in     Base_Type;
        im1 : in     Vkm_GenMatrix) return Vkm_GenMatrix;


end Vulkan.Math.GenMatrix;
