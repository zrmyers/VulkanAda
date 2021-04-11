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
--< @group Vulkan Math GenType
--------------------------------------------------------------------------------
--< @summary
--< This generic package provides constructors, getters and setters, swizzlers,
--< and functors for a generic vector type.
--<
--< @description
--< The Vkm_GenType is a tagged record which contains either a 1D, 2D, 3D, or 4D
--< vector of elements of the generic Base_Type.
--<
--< The components of each vector can be accessed via component names XYZW, RGBA,
--< or STPQ.  Additionally, components of a generic type can be set with these
--< names. The following is an example of getting the second component of one 4D
--< vector and setting a component of another 4D vector:
--<     declare
--<         vec1, vec2 : Vkm_GenType(last_index => 3);
--<     begin
--<         -- Set vec1.x to vec2.y
--<         vec1.x(vec2.y)
--<     end
--<
--< Swizzling can also be done to obtain different permutations of a components
--< from vectors or set different combinations of components in a vector.
--< An example is shown below:
--<    declare
--<        vec1, vec2 : Vkm_GenType(last_index => 3);
--<    begin
--<        -- Set vec1.x to vec2.y and vec1.w to vec2.z.
--<        vec1.xw(vec2.yz)
--<    end
--<
--< The following restrictions apply to swizzling:
--< - A swizzle get or set operation will raise a constraint exception if attempting to
--<   access a component only available for vectors of dimmension greater than
--<   the dimmension of the vector the get operation is being performed on.
--< - A swizzle set cannot be done for combinations that include multiple of the
--<   same component. I.e. vec1.xx(vec2.xy) is not allowed.
--<
--------------------------------------------------------------------------------
generic
    type Base_Type is private;
    Default_Value : in Base_Type;

    ----------------------------------------------------------------------------
    --< @summary
    --< Retrieve the image of the instance of Base_Type.
    --<
    --< @param Instance
    --< The instance of Base_Type.
    --<
    --< @return
    --< The image of base type.
    ----------------------------------------------------------------------------
    with function Image (Instance : in     Base_Type) return String;

    ----------------------------------------------------------------------------
    --< @summary
    --< Unary minus operator for instance of Base_Type.
    --<
    --< @param
    --< The instance of Base_Type
    --<
    --< @return
    --< The complement of the instance of Base_Type.
    ----------------------------------------------------------------------------
    with function Unary_Minus(instance : in     Base_Type) return Base_Type;


    ----------------------------------------------------------------------------
    --< @summary
    --< Multiply operator for two instances of Base_Type.
    --<
    --< @param
    --< The left instance of Base_Type
    --<
    --< @param
    --< The right instance of Base_Type
    --<
    --< @return
    --< The product of two instances of Base_Type.
    ----------------------------------------------------------------------------
    with function Multiply(left, right : in     Base_Type) return Base_Type;

package Vulkan.Math.GenType is
    pragma Preelaborate;
    pragma Pure;

    ----------------------------------------------------------------------------
    -- Types
    ----------------------------------------------------------------------------
    --< The vector type is an array with indices in the range of the Vkm_Indices
    --< type [0 .. 3]. Because of this, the length of any vector instance is
    --< in the range [1 .. 4].
    type Vkm_Vector is array(Vkm_Indices range <>) of aliased Base_Type;
    pragma Convention(C,Vkm_Vector);


    --< The gentype is a discriminant tagged record which encapsulates the
    --< Vkm_Vector type. This allows use of "." to perform functions on an
    --< instance of vector.
    --<
    --< @field last_index
    --< The discriminant, last_index, determines the size of Vkm_GenType's
    --< vector.
    --<
    --< @field data
    --< The vector data for the record. This information is able to be
    --< passed to a C/C++ context.
    type Vkm_GenType(last_index : Vkm_Indices) is tagged
        record
            data : Vkm_Vector(Vkm_Indices'First .. last_index);
        end record;

   --< A reference to a generic vector type. The Vkm_GenType instance is
   --< automatically dereferenced on use.
    type Vkm_GenType_Reference(vector : not null access Vkm_GenType) is null record
        with Implicit_Dereference => vector;

    ----------------------------------------------------------------------------
    -- Operations on Vkm_GenType
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    -- Constructors
    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor of type Vkm_GenType.
    --<
    --< @description
    --< This operation creates an instance of Vkm_GenType with size indicated by
    --< Last_Index, and all components set to a specified value.
    --<
    --< @param last_index
    --< The last index of the vector. Components can be can be accessed using
    --< indices [0 .. last_index].
    --<
    --< @param value
    --< The value that each component of the instance Vkm_GenType should be set to.
    --<
    --< @return
    --< A new instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function Make_GenType (last_index : in     Vkm_Indices;
                           value      : in     Base_Type) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor of type Vkm_GenType.
    --<
    --< @description
    --< This operation creates an instance of Vkm_GenType with one component set
    --< to the indicated value.
    --<
    --< @param value1
    --< The value that the first component of the Vkm_GenType vector is set to.
    --<
    --< @return
    --< A new instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function Make_GenType (value1 : in Base_Type) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor of type Vkm_GenType.
    --<
    --< @description
    --< This operation creates an instance of Vkm_GenType with two components set
    --< to the indicated values.
    --<
    --< @param value1
    --< The value that the first component of the Vkm_GenType vector is set to.
    --<
    --< @param value2
    --< The value that the second component of the Vkm_GenType vector is set to.
    --<
    --< @return
    --< A new instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function Make_GenType (value1, value2 : in Base_Type) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor of type Vkm_GenType.
    --<
    --< @description
    --< This operation creates an instance of Vkm_GenType with three components set
    --< to the indicated values.
    --<
    --< @param value1
    --< The value that the first component of the Vkm_GenType vector is set to.
    --<
    --< @param value2
    --< The value that the second component of the Vkm_GenType vector is set to.
    --<
    --< @param value3
    --< The value that the third component of the Vkm_GenType vector is set to.
    --<
    --< @return
    --< A new instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function Make_GenType (value1, value2, value3 : in Base_Type) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor of type Vkm_GenType.
    --<
    --< @description
    --< This operation creates an instance of Vkm_GenType with four components set
    --< to the indicated values.
    --<
    --< @param value1
    --< The value that the first component of the Vkm_GenType vector is set to.
    --<
    --< @param value2
    --< The value that the second component of the Vkm_GenType vector is set to.
    --<
    --< @param value3
    --< The value that the third component of the Vkm_GenType vector is set to.
    --<
    --< @param value4
    --< The value that the fourth component of the Vkm_GenType vector is set to.
    --<
    --< @return
    --< A new instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function Make_GenType (value1, value2, value3, value4 : in Base_Type) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor of type Vkm_GenType.
    --<
    --< @description
    --< This operation creates an instance of Vkm_GenType with four components set
    --< to the indicated values.
    --<
    --< @param size
    --< The size of the vector to create.
    --<
    --< @param value1
    --< The value that the first component of the Vkm_GenType vector is set to.
    --<
    --< @param value2
    --< The value that the second component of the Vkm_GenType vector is set to.
    --<
    --< @param value3
    --< The value that the third component of the Vkm_GenType vector is set to.
    --<
    --< @param value4
    --< The value that the fourth component of the Vkm_GenType vector is set to.
    --<
    --< @return
    --< A new instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function Make_GenType (
        size                           : in Vkm_Length;
        value1, value2, value3, value4 : in Base_Type := Default_Value) return Vkm_GenType is
        (case size is
            when 1 => Make_GenType(value1),
            when 2 => Make_GenType(value1, value2),
            when 3 => Make_GenType(value1, value2, value3),
            when 4 => Make_GenType(value1, value2, value3, value4)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Constructor of type Vkm_GenType.
    --<
    --< @description
    --< This operation creates an instance of Vkm_GenType by copying an existing
    --< instance of Vkm_GenType. The new instance is of the same dimmension and
    --< value as the instance used for construction.
    --<
    --< @param value
    --< The instance of Vkm_GenType used to initialize the new instance of Vkm_GenType.
    --<
    --< @return
    --< A new instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function Make_GenType (value : in     Vkm_GenType) return Vkm_GenType;

    ----------------------------------------------------------------------------
    --< @summary
    --< The number of components in the instance of Vkm_GenType.
    --<
    --< @description
    --< The number of components in the instance of Vkm_GenType.
    --<
    --< @param instance
    --< The instance of Vkm_GenType.
    --<
    --< @return
    --< The dimmension of the vector.
    ----------------------------------------------------------------------------
    function Length (instance : in     Vkm_GenType) return Vkm_Length is
        (instance.data'Length) with Inline;

    ----------------------------------------------------------------------------
    --< @summary
    --< The image of the vector.
    --<
    --< @description
    --< Generates a human readable string which contains the contents of the
    --< instance of Vkm_GenType. The formatted string appears as follows for
    --< a vector with three components:
    --<
    --<     "[ " & Image(vec.x) & ", " & Image(vec.y) & ", " & Image(vec.z) & " ]"
    --<
    --< The Image() subprogram that is supplied during generic instantiation is used
    --< to print the component of Base_Type.
    --<
    --< @param instance
    --< The instance of Vkm_GenType.
    --<
    --< @return
    --< The human readable image of the vector.
    ----------------------------------------------------------------------------
    function Image (instance : in     Vkm_GenType) return String;

    ----------------------------------------------------------------------------
    --< @summary
    --< Copy components from source to target.
    --<
    --< @description
    --< Copies num_copy components from the source Vkm_GenType instance to the
    --< indicated offset of the destination Vkm_GenType instance.
    --<
    --< @param destination
    --< The instance of Vkm_GenType that component values are copied into.
    --<
    --< @param source
    --< The instance of Vkm_GenType that component values are copied from.
    --<
    --< @param num_copy
    --< The number of components to copy from the source instance.
    --<
    --< @param offset
    --< The offset into the destination into the destination instance at which
    --< components should be copied into.
    ----------------------------------------------------------------------------
    procedure Copy (destination : in out Vkm_GenType;
                    source      : in     Vkm_GenType;
                    num_copy    : in     Vkm_Length;
                    offset      : in     Vkm_Indices);


    ----------------------------------------------------------------------------
    --< @summary
    --< Concatenate two vectors together.
    --<
    --< @description
    --< Creates a new instance of Vkm_GenType which contains components from two
    --< existing instances, left and right. If left and right are both 2 dimmensional
    --< vectors, the new vector is a 4 dimmensional vector described as follows:
    --<
    --<     cat := [ left.x, left.y, right.x , right.y]
    --<
    --< @param left
    --< The instance of Vkm_GenType that is on the left hand side of the concatenation.
    --<
    --< @param right
    --< The instance of Vkm_GenType that is on the right hand side of the concatenation.
    --<
    --< @return
    --< The concatenated vectors.
    ----------------------------------------------------------------------------
    function Concatenate (left, right : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    -- Basic Operations
    ----------------------------------------------------------------------------
    function Equals(left, right : in     Vkm_GenType) return Vkm_Bool;
    function Unary_Plus(instance : in     Vkm_GenType) return Vkm_GenType is
        (instance) with inline;
    function Unary_Minus(instance : in     Vkm_GenType) return Vkm_GenType;
    function Componentwise_Multiply(left, right : in     Vkm_GenType) return Vkm_GenType;
    function Vector_By_Scalar_Multiply(left : in Vkm_GenType;
                                       right : in Base_Type) return Vkm_GenType;

    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access the indicated element of the vector. If the element is out of
    --< bounds for the vector, the Base_Type's default value is returned instead.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @param index
    --< The index of the component to retrieve.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function Component (vec   : in     Vkm_GenType;
                        index : in     Vkm_Indices) return Base_Type;

    procedure Component (vec   : in out Vkm_GenType;
                         index : in     Vkm_Indices;
                         value : in     Base_Type);

    ----------------------------------------------------------------------------
    -- Vector Swizzlers
    ----------------------------------------------------------------------------
    -- 1 D
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an XYZW component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function x (vec : in     Vkm_GenType) return Base_Type is
        (vec.Component(0)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an XYZW component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function y (vec : in     Vkm_GenType) return Base_Type is
        (vec.Component(1)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an XYZW component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function z (vec : in     Vkm_GenType) return Base_Type is
        (vec.Component(2)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an XYZW component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function w (vec : in     Vkm_GenType) return Base_Type is
        (vec.Component(3)) with Inline;

    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an RGBA component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function r (vec : in     Vkm_GenType) return Base_Type renames x;

    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an RGBA component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function g (vec : in     Vkm_GenType) return Base_Type renames y;

    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an RGBA component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function b (vec : in     Vkm_GenType) return Base_Type renames z;

    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an RGBA component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function a (vec : in     Vkm_GenType) return Base_Type renames w;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an STPQ component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function s (vec : in     Vkm_GenType) return Base_Type renames x;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an STPQ component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function t (vec : in     Vkm_GenType) return Base_Type renames y;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an STPQ component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function p (vec : in     Vkm_GenType) return Base_Type renames z;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Read access to a vector component.
    --<
    --< @description
    --< Access an STPQ component of a vector.
    --<
    --< @param vec
    --< The vector to retrieve the component from.
    --<
    --< @return
    --< The value of the indicated component.
    ----------------------------------------------------------------------------
    function q (vec : in     Vkm_GenType) return Base_Type renames w;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a XYZW component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  x (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a XYZW component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  y (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a XYZW component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  z (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a XYZW component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  w (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a RGBA component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  r (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference renames x;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a RGBA component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  g (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference renames y;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a RGBA component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  b (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference renames z;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a RGBA component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  a (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference renames w;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a STPQ component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  s (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference renames x;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a STPQ component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  t (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference renames y;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a STPQ component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  p (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference renames z;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a STPQ component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    --<
    --< @return
    --< A reference to the vector whose component was set.
    ----------------------------------------------------------------------------
    function  q (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_GenType_Reference renames w;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a XYZW component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure x (vec1  : in out Vkm_GenType;
                 value : in    Base_Type  );


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a XYZW component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure y (vec1  : in out Vkm_GenType;
                 value : in    Base_Type  );


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a XYZW component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure z (vec1  : in out Vkm_GenType;
                 value : in    Base_Type  );


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a XYZW component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure w (vec1  : in out Vkm_GenType;
                 value : in    Base_Type  );


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a RGBA component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure r (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames x;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a RGBA component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure g (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames y;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a RGBA component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure b (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames z;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a RGBA component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure a (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames w;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a STPQ component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure s (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames x;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a STPQ component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure t (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames y;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a STPQ component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure p (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames z;


    ----------------------------------------------------------------------------
    --< @private
    --< @summary
    --< Write access to a vector component.
    --<
    --< @description
    --< Set the value of a STPQ component of a vector.
    --<
    --< @param vec1
    --< The vector to set the component of.
    --<
    --< @param value
    --< The value to set the component to.
    ----------------------------------------------------------------------------
    procedure q (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames w;


    ----------------------------------------------------------------------------
    -- 2 D
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function xx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function xy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function xz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function xw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function yx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function yy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function yz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function yw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function zx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function zy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function zz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function zw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function wx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function wy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function wz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function rr (vec : in     Vkm_GenType) return Vkm_GenType renames xx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function rg (vec : in     Vkm_GenType) return Vkm_GenType renames xy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function rb (vec : in     Vkm_GenType) return Vkm_GenType renames xz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ra (vec : in     Vkm_GenType) return Vkm_GenType renames xw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function gr (vec : in     Vkm_GenType) return Vkm_GenType renames yx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function gg (vec : in     Vkm_GenType) return Vkm_GenType renames yy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function gb (vec : in     Vkm_GenType) return Vkm_GenType renames yz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ga (vec : in     Vkm_GenType) return Vkm_GenType renames yw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function br (vec : in     Vkm_GenType) return Vkm_GenType renames zx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function bg (vec : in     Vkm_GenType) return Vkm_GenType renames zy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function bb (vec : in     Vkm_GenType) return Vkm_GenType renames zz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ba (vec : in     Vkm_GenType) return Vkm_GenType renames zw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ar (vec : in     Vkm_GenType) return Vkm_GenType renames wx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ag (vec : in     Vkm_GenType) return Vkm_GenType renames wy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ab (vec : in     Vkm_GenType) return Vkm_GenType renames wz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function aa (vec : in     Vkm_GenType) return Vkm_GenType renames ww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ss (vec : in     Vkm_GenType) return Vkm_GenType renames xx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function st (vec : in     Vkm_GenType) return Vkm_GenType renames xy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function sp (vec : in     Vkm_GenType) return Vkm_GenType renames xz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function sq (vec : in     Vkm_GenType) return Vkm_GenType renames xw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ts (vec : in     Vkm_GenType) return Vkm_GenType renames yx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function tt (vec : in     Vkm_GenType) return Vkm_GenType renames yy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function tp (vec : in     Vkm_GenType) return Vkm_GenType renames yz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function tq (vec : in     Vkm_GenType) return Vkm_GenType renames yw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function ps (vec : in     Vkm_GenType) return Vkm_GenType renames zx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function pt (vec : in     Vkm_GenType) return Vkm_GenType renames zy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function pp (vec : in     Vkm_GenType) return Vkm_GenType renames zz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function pq (vec : in     Vkm_GenType) return Vkm_GenType renames zw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function qs (vec : in     Vkm_GenType) return Vkm_GenType renames wx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function qt (vec : in     Vkm_GenType) return Vkm_GenType renames wy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function qp (vec : in     Vkm_GenType) return Vkm_GenType renames wz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    --<
    --< @param vec
    --< The vector for which to get the 2D swizzle.
    --<
    --< @return
    --< The 2D swizzle from the vector.
    ----------------------------------------------------------------------------
    function qq (vec : in     Vkm_GenType) return Vkm_GenType renames ww;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure xy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure xz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure xw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure yx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure yz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure yw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure zx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure zy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure zw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure wx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure wy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure wz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);




    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure rg (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure rb (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure ra (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure gr (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure gb (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure ga (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure br (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure bg (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure ba (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure ar (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure ag (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure ab (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wz;




    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure st (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure sp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure sq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure ts (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure tp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure tq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure ps (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure pt (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure pq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure qs (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure qt (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    --<
    --< @param vec1
    --< The value for which the swizzle set is performed.
    --<
    --< @param vec2
    --< The 2D swizzle value to set for vec1.
    ----------------------------------------------------------------------------
    procedure qp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wz;

    ----------------------------------------------------------------------------
    -- 3 D
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function xww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function yww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function zww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function wwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function www (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rrr (vec : in     Vkm_GenType) return Vkm_GenType renames xxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rrg (vec : in     Vkm_GenType) return Vkm_GenType renames xxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rrb (vec : in     Vkm_GenType) return Vkm_GenType renames xxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rra (vec : in     Vkm_GenType) return Vkm_GenType renames xxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rgr (vec : in     Vkm_GenType) return Vkm_GenType renames xyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rgg (vec : in     Vkm_GenType) return Vkm_GenType renames xyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rgb (vec : in     Vkm_GenType) return Vkm_GenType renames xyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rga (vec : in     Vkm_GenType) return Vkm_GenType renames xyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rbr (vec : in     Vkm_GenType) return Vkm_GenType renames xzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rbg (vec : in     Vkm_GenType) return Vkm_GenType renames xzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rbb (vec : in     Vkm_GenType) return Vkm_GenType renames xzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rba (vec : in     Vkm_GenType) return Vkm_GenType renames xzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rar (vec : in     Vkm_GenType) return Vkm_GenType renames xwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rag (vec : in     Vkm_GenType) return Vkm_GenType renames xwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function rab (vec : in     Vkm_GenType) return Vkm_GenType renames xwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function raa (vec : in     Vkm_GenType) return Vkm_GenType renames xww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function grr (vec : in     Vkm_GenType) return Vkm_GenType renames yxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function grg (vec : in     Vkm_GenType) return Vkm_GenType renames yxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function grb (vec : in     Vkm_GenType) return Vkm_GenType renames yxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gra (vec : in     Vkm_GenType) return Vkm_GenType renames yxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ggr (vec : in     Vkm_GenType) return Vkm_GenType renames yyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ggg (vec : in     Vkm_GenType) return Vkm_GenType renames yyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ggb (vec : in     Vkm_GenType) return Vkm_GenType renames yyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gga (vec : in     Vkm_GenType) return Vkm_GenType renames yyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gbr (vec : in     Vkm_GenType) return Vkm_GenType renames yzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gbg (vec : in     Vkm_GenType) return Vkm_GenType renames yzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gbb (vec : in     Vkm_GenType) return Vkm_GenType renames yzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gba (vec : in     Vkm_GenType) return Vkm_GenType renames yzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gar (vec : in     Vkm_GenType) return Vkm_GenType renames ywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gag (vec : in     Vkm_GenType) return Vkm_GenType renames ywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gab (vec : in     Vkm_GenType) return Vkm_GenType renames ywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function gaa (vec : in     Vkm_GenType) return Vkm_GenType renames yww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function brr (vec : in     Vkm_GenType) return Vkm_GenType renames zxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function brg (vec : in     Vkm_GenType) return Vkm_GenType renames zxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function brb (vec : in     Vkm_GenType) return Vkm_GenType renames zxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bra (vec : in     Vkm_GenType) return Vkm_GenType renames zxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bgr (vec : in     Vkm_GenType) return Vkm_GenType renames zyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bgg (vec : in     Vkm_GenType) return Vkm_GenType renames zyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bgb (vec : in     Vkm_GenType) return Vkm_GenType renames zyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bga (vec : in     Vkm_GenType) return Vkm_GenType renames zyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bbr (vec : in     Vkm_GenType) return Vkm_GenType renames zzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bbg (vec : in     Vkm_GenType) return Vkm_GenType renames zzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bbb (vec : in     Vkm_GenType) return Vkm_GenType renames zzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bba (vec : in     Vkm_GenType) return Vkm_GenType renames zzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bar (vec : in     Vkm_GenType) return Vkm_GenType renames zwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bag (vec : in     Vkm_GenType) return Vkm_GenType renames zwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function bab (vec : in     Vkm_GenType) return Vkm_GenType renames zwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function baa (vec : in     Vkm_GenType) return Vkm_GenType renames zww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function arr (vec : in     Vkm_GenType) return Vkm_GenType renames wxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function arg (vec : in     Vkm_GenType) return Vkm_GenType renames wxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function arb (vec : in     Vkm_GenType) return Vkm_GenType renames wxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ara (vec : in     Vkm_GenType) return Vkm_GenType renames wxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function agr (vec : in     Vkm_GenType) return Vkm_GenType renames wyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function agg (vec : in     Vkm_GenType) return Vkm_GenType renames wyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function agb (vec : in     Vkm_GenType) return Vkm_GenType renames wyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function aga (vec : in     Vkm_GenType) return Vkm_GenType renames wyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function abr (vec : in     Vkm_GenType) return Vkm_GenType renames wzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function abg (vec : in     Vkm_GenType) return Vkm_GenType renames wzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function abb (vec : in     Vkm_GenType) return Vkm_GenType renames wzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function aba (vec : in     Vkm_GenType) return Vkm_GenType renames wzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function aar (vec : in     Vkm_GenType) return Vkm_GenType renames wwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function aag (vec : in     Vkm_GenType) return Vkm_GenType renames wwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function aab (vec : in     Vkm_GenType) return Vkm_GenType renames wwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function aaa (vec : in     Vkm_GenType) return Vkm_GenType renames www;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function sss (vec : in     Vkm_GenType) return Vkm_GenType renames xxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function sst (vec : in     Vkm_GenType) return Vkm_GenType renames xxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ssp (vec : in     Vkm_GenType) return Vkm_GenType renames xxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ssq (vec : in     Vkm_GenType) return Vkm_GenType renames xxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function sts (vec : in     Vkm_GenType) return Vkm_GenType renames xyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function stt (vec : in     Vkm_GenType) return Vkm_GenType renames xyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function stp (vec : in     Vkm_GenType) return Vkm_GenType renames xyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function stq (vec : in     Vkm_GenType) return Vkm_GenType renames xyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function sps (vec : in     Vkm_GenType) return Vkm_GenType renames xzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function spt (vec : in     Vkm_GenType) return Vkm_GenType renames xzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function spp (vec : in     Vkm_GenType) return Vkm_GenType renames xzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function spq (vec : in     Vkm_GenType) return Vkm_GenType renames xzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function sqs (vec : in     Vkm_GenType) return Vkm_GenType renames xwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function sqt (vec : in     Vkm_GenType) return Vkm_GenType renames xwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function sqp (vec : in     Vkm_GenType) return Vkm_GenType renames xwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function sqq (vec : in     Vkm_GenType) return Vkm_GenType renames xww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tss (vec : in     Vkm_GenType) return Vkm_GenType renames yxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tst (vec : in     Vkm_GenType) return Vkm_GenType renames yxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tsp (vec : in     Vkm_GenType) return Vkm_GenType renames yxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tsq (vec : in     Vkm_GenType) return Vkm_GenType renames yxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tts (vec : in     Vkm_GenType) return Vkm_GenType renames yyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ttt (vec : in     Vkm_GenType) return Vkm_GenType renames yyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ttp (vec : in     Vkm_GenType) return Vkm_GenType renames yyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ttq (vec : in     Vkm_GenType) return Vkm_GenType renames yyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tps (vec : in     Vkm_GenType) return Vkm_GenType renames yzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tpt (vec : in     Vkm_GenType) return Vkm_GenType renames yzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tpp (vec : in     Vkm_GenType) return Vkm_GenType renames yzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tpq (vec : in     Vkm_GenType) return Vkm_GenType renames yzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tqs (vec : in     Vkm_GenType) return Vkm_GenType renames ywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tqt (vec : in     Vkm_GenType) return Vkm_GenType renames ywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tqp (vec : in     Vkm_GenType) return Vkm_GenType renames ywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function tqq (vec : in     Vkm_GenType) return Vkm_GenType renames yww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function pss (vec : in     Vkm_GenType) return Vkm_GenType renames zxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function pst (vec : in     Vkm_GenType) return Vkm_GenType renames zxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function psp (vec : in     Vkm_GenType) return Vkm_GenType renames zxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function psq (vec : in     Vkm_GenType) return Vkm_GenType renames zxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function pts (vec : in     Vkm_GenType) return Vkm_GenType renames zyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ptt (vec : in     Vkm_GenType) return Vkm_GenType renames zyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ptp (vec : in     Vkm_GenType) return Vkm_GenType renames zyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ptq (vec : in     Vkm_GenType) return Vkm_GenType renames zyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function pps (vec : in     Vkm_GenType) return Vkm_GenType renames zzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ppt (vec : in     Vkm_GenType) return Vkm_GenType renames zzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ppp (vec : in     Vkm_GenType) return Vkm_GenType renames zzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function ppq (vec : in     Vkm_GenType) return Vkm_GenType renames zzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function pqs (vec : in     Vkm_GenType) return Vkm_GenType renames zwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function pqt (vec : in     Vkm_GenType) return Vkm_GenType renames zwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function pqp (vec : in     Vkm_GenType) return Vkm_GenType renames zwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function pqq (vec : in     Vkm_GenType) return Vkm_GenType renames zww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qss (vec : in     Vkm_GenType) return Vkm_GenType renames wxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qst (vec : in     Vkm_GenType) return Vkm_GenType renames wxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qsp (vec : in     Vkm_GenType) return Vkm_GenType renames wxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qsq (vec : in     Vkm_GenType) return Vkm_GenType renames wxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qts (vec : in     Vkm_GenType) return Vkm_GenType renames wyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qtt (vec : in     Vkm_GenType) return Vkm_GenType renames wyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qtp (vec : in     Vkm_GenType) return Vkm_GenType renames wyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qtq (vec : in     Vkm_GenType) return Vkm_GenType renames wyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qps (vec : in     Vkm_GenType) return Vkm_GenType renames wzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qpt (vec : in     Vkm_GenType) return Vkm_GenType renames wzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qpp (vec : in     Vkm_GenType) return Vkm_GenType renames wzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qpq (vec : in     Vkm_GenType) return Vkm_GenType renames wzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qqs (vec : in     Vkm_GenType) return Vkm_GenType renames wwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qqt (vec : in     Vkm_GenType) return Vkm_GenType renames wwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qqp (vec : in     Vkm_GenType) return Vkm_GenType renames wwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector.
    --<
    --< @param vec
    --< The value from which to get the 3D swizzle.
    ----------------------------------------------------------------------------
    function qqq (vec : in     Vkm_GenType) return Vkm_GenType renames www;

    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure xyz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure xyw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure xzy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure xzw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure xwy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure xwz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure yxz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure yxw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure yzx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure yzw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure ywx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure ywz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure zxy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure zxw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure zyx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure zyw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure zwx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure zwy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure wxy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure wxz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure wyx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure wyz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure wzx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure wzy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);




    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure rgb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure rga (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure rbg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure rba (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure rag (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure rab (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure grb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure gra (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure gbr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure gba (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure gar (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure gab (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure brg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure bra (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure bgr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure bga (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure bar (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure bag (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure arg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure arb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure agr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure agb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure abr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure abg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure stp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure stq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure spt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure spq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure sqt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure sqp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure tsp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure tsq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure tps (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure tpq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure tqs (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure tqp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure pst (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure psq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure pts (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure ptq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure pqs (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure pqt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure qst (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure qsp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure qts (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure qtp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure qps (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 3D swizzle.
    --<
    --< @param vec2
    --< The 3D swizzle value.
    ----------------------------------------------------------------------------
    procedure qpt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzy;

    ----------------------------------------------------------------------------
    -- 4 D
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function xwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function yzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ywww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function zwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.x)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.y)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.z)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function wwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.w)) with Inline;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrrr (vec : in     Vkm_GenType) return Vkm_GenType renames xxxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrrg (vec : in     Vkm_GenType) return Vkm_GenType renames xxxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrrb (vec : in     Vkm_GenType) return Vkm_GenType renames xxxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrra (vec : in     Vkm_GenType) return Vkm_GenType renames xxxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrgr (vec : in     Vkm_GenType) return Vkm_GenType renames xxyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrgg (vec : in     Vkm_GenType) return Vkm_GenType renames xxyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrgb (vec : in     Vkm_GenType) return Vkm_GenType renames xxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrga (vec : in     Vkm_GenType) return Vkm_GenType renames xxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrbr (vec : in     Vkm_GenType) return Vkm_GenType renames xxzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrbg (vec : in     Vkm_GenType) return Vkm_GenType renames xxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrbb (vec : in     Vkm_GenType) return Vkm_GenType renames xxzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrba (vec : in     Vkm_GenType) return Vkm_GenType renames xxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrar (vec : in     Vkm_GenType) return Vkm_GenType renames xxwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrag (vec : in     Vkm_GenType) return Vkm_GenType renames xxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rrab (vec : in     Vkm_GenType) return Vkm_GenType renames xxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rraa (vec : in     Vkm_GenType) return Vkm_GenType renames xxww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgrr (vec : in     Vkm_GenType) return Vkm_GenType renames xyxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgrg (vec : in     Vkm_GenType) return Vkm_GenType renames xyxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgrb (vec : in     Vkm_GenType) return Vkm_GenType renames xyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgra (vec : in     Vkm_GenType) return Vkm_GenType renames xyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rggr (vec : in     Vkm_GenType) return Vkm_GenType renames xyyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rggg (vec : in     Vkm_GenType) return Vkm_GenType renames xyyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rggb (vec : in     Vkm_GenType) return Vkm_GenType renames xyyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgga (vec : in     Vkm_GenType) return Vkm_GenType renames xyyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgbr (vec : in     Vkm_GenType) return Vkm_GenType renames xyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgbg (vec : in     Vkm_GenType) return Vkm_GenType renames xyzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgbb (vec : in     Vkm_GenType) return Vkm_GenType renames xyzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgba (vec : in     Vkm_GenType) return Vkm_GenType renames xyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgar (vec : in     Vkm_GenType) return Vkm_GenType renames xywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgag (vec : in     Vkm_GenType) return Vkm_GenType renames xywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgab (vec : in     Vkm_GenType) return Vkm_GenType renames xywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rgaa (vec : in     Vkm_GenType) return Vkm_GenType renames xyww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbrr (vec : in     Vkm_GenType) return Vkm_GenType renames xzxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbrg (vec : in     Vkm_GenType) return Vkm_GenType renames xzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbrb (vec : in     Vkm_GenType) return Vkm_GenType renames xzxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbra (vec : in     Vkm_GenType) return Vkm_GenType renames xzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbgr (vec : in     Vkm_GenType) return Vkm_GenType renames xzyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbgg (vec : in     Vkm_GenType) return Vkm_GenType renames xzyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbgb (vec : in     Vkm_GenType) return Vkm_GenType renames xzyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbga (vec : in     Vkm_GenType) return Vkm_GenType renames xzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbbr (vec : in     Vkm_GenType) return Vkm_GenType renames xzzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbbg (vec : in     Vkm_GenType) return Vkm_GenType renames xzzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbbb (vec : in     Vkm_GenType) return Vkm_GenType renames xzzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbba (vec : in     Vkm_GenType) return Vkm_GenType renames xzzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbar (vec : in     Vkm_GenType) return Vkm_GenType renames xzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbag (vec : in     Vkm_GenType) return Vkm_GenType renames xzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbab (vec : in     Vkm_GenType) return Vkm_GenType renames xzwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rbaa (vec : in     Vkm_GenType) return Vkm_GenType renames xzww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rarr (vec : in     Vkm_GenType) return Vkm_GenType renames xwxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rarg (vec : in     Vkm_GenType) return Vkm_GenType renames xwxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rarb (vec : in     Vkm_GenType) return Vkm_GenType renames xwxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rara (vec : in     Vkm_GenType) return Vkm_GenType renames xwxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ragr (vec : in     Vkm_GenType) return Vkm_GenType renames xwyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ragg (vec : in     Vkm_GenType) return Vkm_GenType renames xwyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ragb (vec : in     Vkm_GenType) return Vkm_GenType renames xwyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function raga (vec : in     Vkm_GenType) return Vkm_GenType renames xwyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rabr (vec : in     Vkm_GenType) return Vkm_GenType renames xwzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rabg (vec : in     Vkm_GenType) return Vkm_GenType renames xwzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function rabb (vec : in     Vkm_GenType) return Vkm_GenType renames xwzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function raba (vec : in     Vkm_GenType) return Vkm_GenType renames xwzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function raar (vec : in     Vkm_GenType) return Vkm_GenType renames xwwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function raag (vec : in     Vkm_GenType) return Vkm_GenType renames xwwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function raab (vec : in     Vkm_GenType) return Vkm_GenType renames xwwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function raaa (vec : in     Vkm_GenType) return Vkm_GenType renames xwww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grrr (vec : in     Vkm_GenType) return Vkm_GenType renames yxxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grrg (vec : in     Vkm_GenType) return Vkm_GenType renames yxxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grrb (vec : in     Vkm_GenType) return Vkm_GenType renames yxxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grra (vec : in     Vkm_GenType) return Vkm_GenType renames yxxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grgr (vec : in     Vkm_GenType) return Vkm_GenType renames yxyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grgg (vec : in     Vkm_GenType) return Vkm_GenType renames yxyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grgb (vec : in     Vkm_GenType) return Vkm_GenType renames yxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grga (vec : in     Vkm_GenType) return Vkm_GenType renames yxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grbr (vec : in     Vkm_GenType) return Vkm_GenType renames yxzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grbg (vec : in     Vkm_GenType) return Vkm_GenType renames yxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grbb (vec : in     Vkm_GenType) return Vkm_GenType renames yxzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grba (vec : in     Vkm_GenType) return Vkm_GenType renames yxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grar (vec : in     Vkm_GenType) return Vkm_GenType renames yxwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grag (vec : in     Vkm_GenType) return Vkm_GenType renames yxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function grab (vec : in     Vkm_GenType) return Vkm_GenType renames yxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function graa (vec : in     Vkm_GenType) return Vkm_GenType renames yxww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggrr (vec : in     Vkm_GenType) return Vkm_GenType renames yyxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggrg (vec : in     Vkm_GenType) return Vkm_GenType renames yyxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggrb (vec : in     Vkm_GenType) return Vkm_GenType renames yyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggra (vec : in     Vkm_GenType) return Vkm_GenType renames yyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gggr (vec : in     Vkm_GenType) return Vkm_GenType renames yyyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gggg (vec : in     Vkm_GenType) return Vkm_GenType renames yyyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gggb (vec : in     Vkm_GenType) return Vkm_GenType renames yyyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggga (vec : in     Vkm_GenType) return Vkm_GenType renames yyyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggbr (vec : in     Vkm_GenType) return Vkm_GenType renames yyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggbg (vec : in     Vkm_GenType) return Vkm_GenType renames yyzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggbb (vec : in     Vkm_GenType) return Vkm_GenType renames yyzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggba (vec : in     Vkm_GenType) return Vkm_GenType renames yyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggar (vec : in     Vkm_GenType) return Vkm_GenType renames yywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggag (vec : in     Vkm_GenType) return Vkm_GenType renames yywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggab (vec : in     Vkm_GenType) return Vkm_GenType renames yywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ggaa (vec : in     Vkm_GenType) return Vkm_GenType renames yyww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbrr (vec : in     Vkm_GenType) return Vkm_GenType renames yzxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbrg (vec : in     Vkm_GenType) return Vkm_GenType renames yzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbrb (vec : in     Vkm_GenType) return Vkm_GenType renames yzxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbra (vec : in     Vkm_GenType) return Vkm_GenType renames yzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbgr (vec : in     Vkm_GenType) return Vkm_GenType renames yzyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbgg (vec : in     Vkm_GenType) return Vkm_GenType renames yzyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbgb (vec : in     Vkm_GenType) return Vkm_GenType renames yzyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbga (vec : in     Vkm_GenType) return Vkm_GenType renames yzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbbr (vec : in     Vkm_GenType) return Vkm_GenType renames yzzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbbg (vec : in     Vkm_GenType) return Vkm_GenType renames yzzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbbb (vec : in     Vkm_GenType) return Vkm_GenType renames yzzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbba (vec : in     Vkm_GenType) return Vkm_GenType renames yzzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbar (vec : in     Vkm_GenType) return Vkm_GenType renames yzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbag (vec : in     Vkm_GenType) return Vkm_GenType renames yzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbab (vec : in     Vkm_GenType) return Vkm_GenType renames yzwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gbaa (vec : in     Vkm_GenType) return Vkm_GenType renames yzww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function garr (vec : in     Vkm_GenType) return Vkm_GenType renames ywxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function garg (vec : in     Vkm_GenType) return Vkm_GenType renames ywxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function garb (vec : in     Vkm_GenType) return Vkm_GenType renames ywxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gara (vec : in     Vkm_GenType) return Vkm_GenType renames ywxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gagr (vec : in     Vkm_GenType) return Vkm_GenType renames ywyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gagg (vec : in     Vkm_GenType) return Vkm_GenType renames ywyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gagb (vec : in     Vkm_GenType) return Vkm_GenType renames ywyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gaga (vec : in     Vkm_GenType) return Vkm_GenType renames ywyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gabr (vec : in     Vkm_GenType) return Vkm_GenType renames ywzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gabg (vec : in     Vkm_GenType) return Vkm_GenType renames ywzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gabb (vec : in     Vkm_GenType) return Vkm_GenType renames ywzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gaba (vec : in     Vkm_GenType) return Vkm_GenType renames ywzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gaar (vec : in     Vkm_GenType) return Vkm_GenType renames ywwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gaag (vec : in     Vkm_GenType) return Vkm_GenType renames ywwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gaab (vec : in     Vkm_GenType) return Vkm_GenType renames ywwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function gaaa (vec : in     Vkm_GenType) return Vkm_GenType renames ywww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brrr (vec : in     Vkm_GenType) return Vkm_GenType renames zxxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brrg (vec : in     Vkm_GenType) return Vkm_GenType renames zxxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brrb (vec : in     Vkm_GenType) return Vkm_GenType renames zxxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brra (vec : in     Vkm_GenType) return Vkm_GenType renames zxxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brgr (vec : in     Vkm_GenType) return Vkm_GenType renames zxyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brgg (vec : in     Vkm_GenType) return Vkm_GenType renames zxyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brgb (vec : in     Vkm_GenType) return Vkm_GenType renames zxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brga (vec : in     Vkm_GenType) return Vkm_GenType renames zxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brbr (vec : in     Vkm_GenType) return Vkm_GenType renames zxzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brbg (vec : in     Vkm_GenType) return Vkm_GenType renames zxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brbb (vec : in     Vkm_GenType) return Vkm_GenType renames zxzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brba (vec : in     Vkm_GenType) return Vkm_GenType renames zxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brar (vec : in     Vkm_GenType) return Vkm_GenType renames zxwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brag (vec : in     Vkm_GenType) return Vkm_GenType renames zxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function brab (vec : in     Vkm_GenType) return Vkm_GenType renames zxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function braa (vec : in     Vkm_GenType) return Vkm_GenType renames zxww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgrr (vec : in     Vkm_GenType) return Vkm_GenType renames zyxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgrg (vec : in     Vkm_GenType) return Vkm_GenType renames zyxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgrb (vec : in     Vkm_GenType) return Vkm_GenType renames zyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgra (vec : in     Vkm_GenType) return Vkm_GenType renames zyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bggr (vec : in     Vkm_GenType) return Vkm_GenType renames zyyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bggg (vec : in     Vkm_GenType) return Vkm_GenType renames zyyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bggb (vec : in     Vkm_GenType) return Vkm_GenType renames zyyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgga (vec : in     Vkm_GenType) return Vkm_GenType renames zyyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgbr (vec : in     Vkm_GenType) return Vkm_GenType renames zyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgbg (vec : in     Vkm_GenType) return Vkm_GenType renames zyzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgbb (vec : in     Vkm_GenType) return Vkm_GenType renames zyzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgba (vec : in     Vkm_GenType) return Vkm_GenType renames zyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgar (vec : in     Vkm_GenType) return Vkm_GenType renames zywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgag (vec : in     Vkm_GenType) return Vkm_GenType renames zywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgab (vec : in     Vkm_GenType) return Vkm_GenType renames zywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bgaa (vec : in     Vkm_GenType) return Vkm_GenType renames zyww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbrr (vec : in     Vkm_GenType) return Vkm_GenType renames zzxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbrg (vec : in     Vkm_GenType) return Vkm_GenType renames zzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbrb (vec : in     Vkm_GenType) return Vkm_GenType renames zzxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbra (vec : in     Vkm_GenType) return Vkm_GenType renames zzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbgr (vec : in     Vkm_GenType) return Vkm_GenType renames zzyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbgg (vec : in     Vkm_GenType) return Vkm_GenType renames zzyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbgb (vec : in     Vkm_GenType) return Vkm_GenType renames zzyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbga (vec : in     Vkm_GenType) return Vkm_GenType renames zzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbbr (vec : in     Vkm_GenType) return Vkm_GenType renames zzzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbbg (vec : in     Vkm_GenType) return Vkm_GenType renames zzzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbbb (vec : in     Vkm_GenType) return Vkm_GenType renames zzzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbba (vec : in     Vkm_GenType) return Vkm_GenType renames zzzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbar (vec : in     Vkm_GenType) return Vkm_GenType renames zzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbag (vec : in     Vkm_GenType) return Vkm_GenType renames zzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbab (vec : in     Vkm_GenType) return Vkm_GenType renames zzwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bbaa (vec : in     Vkm_GenType) return Vkm_GenType renames zzww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function barr (vec : in     Vkm_GenType) return Vkm_GenType renames zwxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function barg (vec : in     Vkm_GenType) return Vkm_GenType renames zwxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function barb (vec : in     Vkm_GenType) return Vkm_GenType renames zwxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bara (vec : in     Vkm_GenType) return Vkm_GenType renames zwxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bagr (vec : in     Vkm_GenType) return Vkm_GenType renames zwyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bagg (vec : in     Vkm_GenType) return Vkm_GenType renames zwyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function bagb (vec : in     Vkm_GenType) return Vkm_GenType renames zwyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function baga (vec : in     Vkm_GenType) return Vkm_GenType renames zwyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function babr (vec : in     Vkm_GenType) return Vkm_GenType renames zwzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function babg (vec : in     Vkm_GenType) return Vkm_GenType renames zwzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function babb (vec : in     Vkm_GenType) return Vkm_GenType renames zwzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function baba (vec : in     Vkm_GenType) return Vkm_GenType renames zwzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function baar (vec : in     Vkm_GenType) return Vkm_GenType renames zwwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function baag (vec : in     Vkm_GenType) return Vkm_GenType renames zwwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function baab (vec : in     Vkm_GenType) return Vkm_GenType renames zwwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function baaa (vec : in     Vkm_GenType) return Vkm_GenType renames zwww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arrr (vec : in     Vkm_GenType) return Vkm_GenType renames wxxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arrg (vec : in     Vkm_GenType) return Vkm_GenType renames wxxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arrb (vec : in     Vkm_GenType) return Vkm_GenType renames wxxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arra (vec : in     Vkm_GenType) return Vkm_GenType renames wxxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function argr (vec : in     Vkm_GenType) return Vkm_GenType renames wxyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function argg (vec : in     Vkm_GenType) return Vkm_GenType renames wxyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function argb (vec : in     Vkm_GenType) return Vkm_GenType renames wxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arga (vec : in     Vkm_GenType) return Vkm_GenType renames wxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arbr (vec : in     Vkm_GenType) return Vkm_GenType renames wxzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arbg (vec : in     Vkm_GenType) return Vkm_GenType renames wxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arbb (vec : in     Vkm_GenType) return Vkm_GenType renames wxzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arba (vec : in     Vkm_GenType) return Vkm_GenType renames wxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arar (vec : in     Vkm_GenType) return Vkm_GenType renames wxwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arag (vec : in     Vkm_GenType) return Vkm_GenType renames wxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function arab (vec : in     Vkm_GenType) return Vkm_GenType renames wxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function araa (vec : in     Vkm_GenType) return Vkm_GenType renames wxww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agrr (vec : in     Vkm_GenType) return Vkm_GenType renames wyxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agrg (vec : in     Vkm_GenType) return Vkm_GenType renames wyxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agrb (vec : in     Vkm_GenType) return Vkm_GenType renames wyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agra (vec : in     Vkm_GenType) return Vkm_GenType renames wyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aggr (vec : in     Vkm_GenType) return Vkm_GenType renames wyyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aggg (vec : in     Vkm_GenType) return Vkm_GenType renames wyyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aggb (vec : in     Vkm_GenType) return Vkm_GenType renames wyyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agga (vec : in     Vkm_GenType) return Vkm_GenType renames wyyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agbr (vec : in     Vkm_GenType) return Vkm_GenType renames wyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agbg (vec : in     Vkm_GenType) return Vkm_GenType renames wyzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agbb (vec : in     Vkm_GenType) return Vkm_GenType renames wyzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agba (vec : in     Vkm_GenType) return Vkm_GenType renames wyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agar (vec : in     Vkm_GenType) return Vkm_GenType renames wywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agag (vec : in     Vkm_GenType) return Vkm_GenType renames wywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agab (vec : in     Vkm_GenType) return Vkm_GenType renames wywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function agaa (vec : in     Vkm_GenType) return Vkm_GenType renames wyww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abrr (vec : in     Vkm_GenType) return Vkm_GenType renames wzxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abrg (vec : in     Vkm_GenType) return Vkm_GenType renames wzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abrb (vec : in     Vkm_GenType) return Vkm_GenType renames wzxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abra (vec : in     Vkm_GenType) return Vkm_GenType renames wzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abgr (vec : in     Vkm_GenType) return Vkm_GenType renames wzyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abgg (vec : in     Vkm_GenType) return Vkm_GenType renames wzyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abgb (vec : in     Vkm_GenType) return Vkm_GenType renames wzyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abga (vec : in     Vkm_GenType) return Vkm_GenType renames wzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abbr (vec : in     Vkm_GenType) return Vkm_GenType renames wzzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abbg (vec : in     Vkm_GenType) return Vkm_GenType renames wzzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abbb (vec : in     Vkm_GenType) return Vkm_GenType renames wzzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abba (vec : in     Vkm_GenType) return Vkm_GenType renames wzzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abar (vec : in     Vkm_GenType) return Vkm_GenType renames wzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abag (vec : in     Vkm_GenType) return Vkm_GenType renames wzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abab (vec : in     Vkm_GenType) return Vkm_GenType renames wzwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function abaa (vec : in     Vkm_GenType) return Vkm_GenType renames wzww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aarr (vec : in     Vkm_GenType) return Vkm_GenType renames wwxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aarg (vec : in     Vkm_GenType) return Vkm_GenType renames wwxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aarb (vec : in     Vkm_GenType) return Vkm_GenType renames wwxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aara (vec : in     Vkm_GenType) return Vkm_GenType renames wwxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aagr (vec : in     Vkm_GenType) return Vkm_GenType renames wwyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aagg (vec : in     Vkm_GenType) return Vkm_GenType renames wwyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aagb (vec : in     Vkm_GenType) return Vkm_GenType renames wwyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aaga (vec : in     Vkm_GenType) return Vkm_GenType renames wwyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aabr (vec : in     Vkm_GenType) return Vkm_GenType renames wwzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aabg (vec : in     Vkm_GenType) return Vkm_GenType renames wwzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aabb (vec : in     Vkm_GenType) return Vkm_GenType renames wwzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aaba (vec : in     Vkm_GenType) return Vkm_GenType renames wwzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aaar (vec : in     Vkm_GenType) return Vkm_GenType renames wwwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aaag (vec : in     Vkm_GenType) return Vkm_GenType renames wwwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aaab (vec : in     Vkm_GenType) return Vkm_GenType renames wwwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function aaaa (vec : in     Vkm_GenType) return Vkm_GenType renames wwww;



    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ssss (vec : in     Vkm_GenType) return Vkm_GenType renames xxxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ssst (vec : in     Vkm_GenType) return Vkm_GenType renames xxxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sssp (vec : in     Vkm_GenType) return Vkm_GenType renames xxxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sssq (vec : in     Vkm_GenType) return Vkm_GenType renames xxxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ssts (vec : in     Vkm_GenType) return Vkm_GenType renames xxyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sstt (vec : in     Vkm_GenType) return Vkm_GenType renames xxyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sstp (vec : in     Vkm_GenType) return Vkm_GenType renames xxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sstq (vec : in     Vkm_GenType) return Vkm_GenType renames xxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ssps (vec : in     Vkm_GenType) return Vkm_GenType renames xxzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sspt (vec : in     Vkm_GenType) return Vkm_GenType renames xxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sspp (vec : in     Vkm_GenType) return Vkm_GenType renames xxzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sspq (vec : in     Vkm_GenType) return Vkm_GenType renames xxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ssqs (vec : in     Vkm_GenType) return Vkm_GenType renames xxwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ssqt (vec : in     Vkm_GenType) return Vkm_GenType renames xxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ssqp (vec : in     Vkm_GenType) return Vkm_GenType renames xxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ssqq (vec : in     Vkm_GenType) return Vkm_GenType renames xxww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stss (vec : in     Vkm_GenType) return Vkm_GenType renames xyxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stst (vec : in     Vkm_GenType) return Vkm_GenType renames xyxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stsp (vec : in     Vkm_GenType) return Vkm_GenType renames xyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stsq (vec : in     Vkm_GenType) return Vkm_GenType renames xyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stts (vec : in     Vkm_GenType) return Vkm_GenType renames xyyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sttt (vec : in     Vkm_GenType) return Vkm_GenType renames xyyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sttp (vec : in     Vkm_GenType) return Vkm_GenType renames xyyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sttq (vec : in     Vkm_GenType) return Vkm_GenType renames xyyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stps (vec : in     Vkm_GenType) return Vkm_GenType renames xyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stpt (vec : in     Vkm_GenType) return Vkm_GenType renames xyzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stpp (vec : in     Vkm_GenType) return Vkm_GenType renames xyzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stpq (vec : in     Vkm_GenType) return Vkm_GenType renames xyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stqs (vec : in     Vkm_GenType) return Vkm_GenType renames xywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stqt (vec : in     Vkm_GenType) return Vkm_GenType renames xywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stqp (vec : in     Vkm_GenType) return Vkm_GenType renames xywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function stqq (vec : in     Vkm_GenType) return Vkm_GenType renames xyww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spss (vec : in     Vkm_GenType) return Vkm_GenType renames xzxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spst (vec : in     Vkm_GenType) return Vkm_GenType renames xzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spsp (vec : in     Vkm_GenType) return Vkm_GenType renames xzxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spsq (vec : in     Vkm_GenType) return Vkm_GenType renames xzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spts (vec : in     Vkm_GenType) return Vkm_GenType renames xzyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sptt (vec : in     Vkm_GenType) return Vkm_GenType renames xzyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sptp (vec : in     Vkm_GenType) return Vkm_GenType renames xzyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sptq (vec : in     Vkm_GenType) return Vkm_GenType renames xzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spps (vec : in     Vkm_GenType) return Vkm_GenType renames xzzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sppt (vec : in     Vkm_GenType) return Vkm_GenType renames xzzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sppp (vec : in     Vkm_GenType) return Vkm_GenType renames xzzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sppq (vec : in     Vkm_GenType) return Vkm_GenType renames xzzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spqs (vec : in     Vkm_GenType) return Vkm_GenType renames xzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spqt (vec : in     Vkm_GenType) return Vkm_GenType renames xzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spqp (vec : in     Vkm_GenType) return Vkm_GenType renames xzwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function spqq (vec : in     Vkm_GenType) return Vkm_GenType renames xzww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqss (vec : in     Vkm_GenType) return Vkm_GenType renames xwxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqst (vec : in     Vkm_GenType) return Vkm_GenType renames xwxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqsp (vec : in     Vkm_GenType) return Vkm_GenType renames xwxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqsq (vec : in     Vkm_GenType) return Vkm_GenType renames xwxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqts (vec : in     Vkm_GenType) return Vkm_GenType renames xwyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqtt (vec : in     Vkm_GenType) return Vkm_GenType renames xwyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqtp (vec : in     Vkm_GenType) return Vkm_GenType renames xwyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqtq (vec : in     Vkm_GenType) return Vkm_GenType renames xwyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqps (vec : in     Vkm_GenType) return Vkm_GenType renames xwzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqpt (vec : in     Vkm_GenType) return Vkm_GenType renames xwzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqpp (vec : in     Vkm_GenType) return Vkm_GenType renames xwzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqpq (vec : in     Vkm_GenType) return Vkm_GenType renames xwzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqqs (vec : in     Vkm_GenType) return Vkm_GenType renames xwwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqqt (vec : in     Vkm_GenType) return Vkm_GenType renames xwwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqqp (vec : in     Vkm_GenType) return Vkm_GenType renames xwwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function sqqq (vec : in     Vkm_GenType) return Vkm_GenType renames xwww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tsss (vec : in     Vkm_GenType) return Vkm_GenType renames yxxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tsst (vec : in     Vkm_GenType) return Vkm_GenType renames yxxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tssp (vec : in     Vkm_GenType) return Vkm_GenType renames yxxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tssq (vec : in     Vkm_GenType) return Vkm_GenType renames yxxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tsts (vec : in     Vkm_GenType) return Vkm_GenType renames yxyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tstt (vec : in     Vkm_GenType) return Vkm_GenType renames yxyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tstp (vec : in     Vkm_GenType) return Vkm_GenType renames yxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tstq (vec : in     Vkm_GenType) return Vkm_GenType renames yxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tsps (vec : in     Vkm_GenType) return Vkm_GenType renames yxzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tspt (vec : in     Vkm_GenType) return Vkm_GenType renames yxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tspp (vec : in     Vkm_GenType) return Vkm_GenType renames yxzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tspq (vec : in     Vkm_GenType) return Vkm_GenType renames yxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tsqs (vec : in     Vkm_GenType) return Vkm_GenType renames yxwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tsqt (vec : in     Vkm_GenType) return Vkm_GenType renames yxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tsqp (vec : in     Vkm_GenType) return Vkm_GenType renames yxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tsqq (vec : in     Vkm_GenType) return Vkm_GenType renames yxww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttss (vec : in     Vkm_GenType) return Vkm_GenType renames yyxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttst (vec : in     Vkm_GenType) return Vkm_GenType renames yyxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttsp (vec : in     Vkm_GenType) return Vkm_GenType renames yyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttsq (vec : in     Vkm_GenType) return Vkm_GenType renames yyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttts (vec : in     Vkm_GenType) return Vkm_GenType renames yyyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tttt (vec : in     Vkm_GenType) return Vkm_GenType renames yyyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tttp (vec : in     Vkm_GenType) return Vkm_GenType renames yyyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tttq (vec : in     Vkm_GenType) return Vkm_GenType renames yyyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttps (vec : in     Vkm_GenType) return Vkm_GenType renames yyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttpt (vec : in     Vkm_GenType) return Vkm_GenType renames yyzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttpp (vec : in     Vkm_GenType) return Vkm_GenType renames yyzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttpq (vec : in     Vkm_GenType) return Vkm_GenType renames yyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttqs (vec : in     Vkm_GenType) return Vkm_GenType renames yywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttqt (vec : in     Vkm_GenType) return Vkm_GenType renames yywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttqp (vec : in     Vkm_GenType) return Vkm_GenType renames yywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ttqq (vec : in     Vkm_GenType) return Vkm_GenType renames yyww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpss (vec : in     Vkm_GenType) return Vkm_GenType renames yzxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpst (vec : in     Vkm_GenType) return Vkm_GenType renames yzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpsp (vec : in     Vkm_GenType) return Vkm_GenType renames yzxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpsq (vec : in     Vkm_GenType) return Vkm_GenType renames yzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpts (vec : in     Vkm_GenType) return Vkm_GenType renames yzyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tptt (vec : in     Vkm_GenType) return Vkm_GenType renames yzyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tptp (vec : in     Vkm_GenType) return Vkm_GenType renames yzyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tptq (vec : in     Vkm_GenType) return Vkm_GenType renames yzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpps (vec : in     Vkm_GenType) return Vkm_GenType renames yzzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tppt (vec : in     Vkm_GenType) return Vkm_GenType renames yzzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tppp (vec : in     Vkm_GenType) return Vkm_GenType renames yzzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tppq (vec : in     Vkm_GenType) return Vkm_GenType renames yzzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpqs (vec : in     Vkm_GenType) return Vkm_GenType renames yzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpqt (vec : in     Vkm_GenType) return Vkm_GenType renames yzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpqp (vec : in     Vkm_GenType) return Vkm_GenType renames yzwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tpqq (vec : in     Vkm_GenType) return Vkm_GenType renames yzww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqss (vec : in     Vkm_GenType) return Vkm_GenType renames ywxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqst (vec : in     Vkm_GenType) return Vkm_GenType renames ywxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqsp (vec : in     Vkm_GenType) return Vkm_GenType renames ywxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqsq (vec : in     Vkm_GenType) return Vkm_GenType renames ywxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqts (vec : in     Vkm_GenType) return Vkm_GenType renames ywyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqtt (vec : in     Vkm_GenType) return Vkm_GenType renames ywyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqtp (vec : in     Vkm_GenType) return Vkm_GenType renames ywyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqtq (vec : in     Vkm_GenType) return Vkm_GenType renames ywyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqps (vec : in     Vkm_GenType) return Vkm_GenType renames ywzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqpt (vec : in     Vkm_GenType) return Vkm_GenType renames ywzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqpp (vec : in     Vkm_GenType) return Vkm_GenType renames ywzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqpq (vec : in     Vkm_GenType) return Vkm_GenType renames ywzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqqs (vec : in     Vkm_GenType) return Vkm_GenType renames ywwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqqt (vec : in     Vkm_GenType) return Vkm_GenType renames ywwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqqp (vec : in     Vkm_GenType) return Vkm_GenType renames ywwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function tqqq (vec : in     Vkm_GenType) return Vkm_GenType renames ywww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function psss (vec : in     Vkm_GenType) return Vkm_GenType renames zxxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function psst (vec : in     Vkm_GenType) return Vkm_GenType renames zxxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pssp (vec : in     Vkm_GenType) return Vkm_GenType renames zxxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pssq (vec : in     Vkm_GenType) return Vkm_GenType renames zxxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function psts (vec : in     Vkm_GenType) return Vkm_GenType renames zxyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pstt (vec : in     Vkm_GenType) return Vkm_GenType renames zxyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pstp (vec : in     Vkm_GenType) return Vkm_GenType renames zxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pstq (vec : in     Vkm_GenType) return Vkm_GenType renames zxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function psps (vec : in     Vkm_GenType) return Vkm_GenType renames zxzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pspt (vec : in     Vkm_GenType) return Vkm_GenType renames zxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pspp (vec : in     Vkm_GenType) return Vkm_GenType renames zxzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pspq (vec : in     Vkm_GenType) return Vkm_GenType renames zxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function psqs (vec : in     Vkm_GenType) return Vkm_GenType renames zxwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function psqt (vec : in     Vkm_GenType) return Vkm_GenType renames zxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function psqp (vec : in     Vkm_GenType) return Vkm_GenType renames zxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function psqq (vec : in     Vkm_GenType) return Vkm_GenType renames zxww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptss (vec : in     Vkm_GenType) return Vkm_GenType renames zyxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptst (vec : in     Vkm_GenType) return Vkm_GenType renames zyxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptsp (vec : in     Vkm_GenType) return Vkm_GenType renames zyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptsq (vec : in     Vkm_GenType) return Vkm_GenType renames zyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptts (vec : in     Vkm_GenType) return Vkm_GenType renames zyyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pttt (vec : in     Vkm_GenType) return Vkm_GenType renames zyyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pttp (vec : in     Vkm_GenType) return Vkm_GenType renames zyyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pttq (vec : in     Vkm_GenType) return Vkm_GenType renames zyyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptps (vec : in     Vkm_GenType) return Vkm_GenType renames zyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptpt (vec : in     Vkm_GenType) return Vkm_GenType renames zyzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptpp (vec : in     Vkm_GenType) return Vkm_GenType renames zyzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptpq (vec : in     Vkm_GenType) return Vkm_GenType renames zyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptqs (vec : in     Vkm_GenType) return Vkm_GenType renames zywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptqt (vec : in     Vkm_GenType) return Vkm_GenType renames zywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptqp (vec : in     Vkm_GenType) return Vkm_GenType renames zywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ptqq (vec : in     Vkm_GenType) return Vkm_GenType renames zyww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppss (vec : in     Vkm_GenType) return Vkm_GenType renames zzxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppst (vec : in     Vkm_GenType) return Vkm_GenType renames zzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppsp (vec : in     Vkm_GenType) return Vkm_GenType renames zzxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppsq (vec : in     Vkm_GenType) return Vkm_GenType renames zzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppts (vec : in     Vkm_GenType) return Vkm_GenType renames zzyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pptt (vec : in     Vkm_GenType) return Vkm_GenType renames zzyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pptp (vec : in     Vkm_GenType) return Vkm_GenType renames zzyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pptq (vec : in     Vkm_GenType) return Vkm_GenType renames zzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppps (vec : in     Vkm_GenType) return Vkm_GenType renames zzzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pppt (vec : in     Vkm_GenType) return Vkm_GenType renames zzzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pppp (vec : in     Vkm_GenType) return Vkm_GenType renames zzzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pppq (vec : in     Vkm_GenType) return Vkm_GenType renames zzzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppqs (vec : in     Vkm_GenType) return Vkm_GenType renames zzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppqt (vec : in     Vkm_GenType) return Vkm_GenType renames zzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppqp (vec : in     Vkm_GenType) return Vkm_GenType renames zzwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function ppqq (vec : in     Vkm_GenType) return Vkm_GenType renames zzww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqss (vec : in     Vkm_GenType) return Vkm_GenType renames zwxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqst (vec : in     Vkm_GenType) return Vkm_GenType renames zwxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqsp (vec : in     Vkm_GenType) return Vkm_GenType renames zwxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqsq (vec : in     Vkm_GenType) return Vkm_GenType renames zwxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqts (vec : in     Vkm_GenType) return Vkm_GenType renames zwyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqtt (vec : in     Vkm_GenType) return Vkm_GenType renames zwyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqtp (vec : in     Vkm_GenType) return Vkm_GenType renames zwyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqtq (vec : in     Vkm_GenType) return Vkm_GenType renames zwyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqps (vec : in     Vkm_GenType) return Vkm_GenType renames zwzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqpt (vec : in     Vkm_GenType) return Vkm_GenType renames zwzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqpp (vec : in     Vkm_GenType) return Vkm_GenType renames zwzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqpq (vec : in     Vkm_GenType) return Vkm_GenType renames zwzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqqs (vec : in     Vkm_GenType) return Vkm_GenType renames zwwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqqt (vec : in     Vkm_GenType) return Vkm_GenType renames zwwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqqp (vec : in     Vkm_GenType) return Vkm_GenType renames zwwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function pqqq (vec : in     Vkm_GenType) return Vkm_GenType renames zwww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qsss (vec : in     Vkm_GenType) return Vkm_GenType renames wxxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qsst (vec : in     Vkm_GenType) return Vkm_GenType renames wxxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qssp (vec : in     Vkm_GenType) return Vkm_GenType renames wxxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qssq (vec : in     Vkm_GenType) return Vkm_GenType renames wxxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qsts (vec : in     Vkm_GenType) return Vkm_GenType renames wxyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qstt (vec : in     Vkm_GenType) return Vkm_GenType renames wxyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qstp (vec : in     Vkm_GenType) return Vkm_GenType renames wxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qstq (vec : in     Vkm_GenType) return Vkm_GenType renames wxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qsps (vec : in     Vkm_GenType) return Vkm_GenType renames wxzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qspt (vec : in     Vkm_GenType) return Vkm_GenType renames wxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qspp (vec : in     Vkm_GenType) return Vkm_GenType renames wxzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qspq (vec : in     Vkm_GenType) return Vkm_GenType renames wxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qsqs (vec : in     Vkm_GenType) return Vkm_GenType renames wxwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qsqt (vec : in     Vkm_GenType) return Vkm_GenType renames wxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qsqp (vec : in     Vkm_GenType) return Vkm_GenType renames wxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qsqq (vec : in     Vkm_GenType) return Vkm_GenType renames wxww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtss (vec : in     Vkm_GenType) return Vkm_GenType renames wyxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtst (vec : in     Vkm_GenType) return Vkm_GenType renames wyxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtsp (vec : in     Vkm_GenType) return Vkm_GenType renames wyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtsq (vec : in     Vkm_GenType) return Vkm_GenType renames wyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtts (vec : in     Vkm_GenType) return Vkm_GenType renames wyyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qttt (vec : in     Vkm_GenType) return Vkm_GenType renames wyyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qttp (vec : in     Vkm_GenType) return Vkm_GenType renames wyyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qttq (vec : in     Vkm_GenType) return Vkm_GenType renames wyyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtps (vec : in     Vkm_GenType) return Vkm_GenType renames wyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtpt (vec : in     Vkm_GenType) return Vkm_GenType renames wyzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtpp (vec : in     Vkm_GenType) return Vkm_GenType renames wyzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtpq (vec : in     Vkm_GenType) return Vkm_GenType renames wyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtqs (vec : in     Vkm_GenType) return Vkm_GenType renames wywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtqt (vec : in     Vkm_GenType) return Vkm_GenType renames wywy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtqp (vec : in     Vkm_GenType) return Vkm_GenType renames wywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qtqq (vec : in     Vkm_GenType) return Vkm_GenType renames wyww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpss (vec : in     Vkm_GenType) return Vkm_GenType renames wzxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpst (vec : in     Vkm_GenType) return Vkm_GenType renames wzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpsp (vec : in     Vkm_GenType) return Vkm_GenType renames wzxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpsq (vec : in     Vkm_GenType) return Vkm_GenType renames wzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpts (vec : in     Vkm_GenType) return Vkm_GenType renames wzyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qptt (vec : in     Vkm_GenType) return Vkm_GenType renames wzyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qptp (vec : in     Vkm_GenType) return Vkm_GenType renames wzyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qptq (vec : in     Vkm_GenType) return Vkm_GenType renames wzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpps (vec : in     Vkm_GenType) return Vkm_GenType renames wzzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qppt (vec : in     Vkm_GenType) return Vkm_GenType renames wzzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qppp (vec : in     Vkm_GenType) return Vkm_GenType renames wzzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qppq (vec : in     Vkm_GenType) return Vkm_GenType renames wzzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpqs (vec : in     Vkm_GenType) return Vkm_GenType renames wzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpqt (vec : in     Vkm_GenType) return Vkm_GenType renames wzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpqp (vec : in     Vkm_GenType) return Vkm_GenType renames wzwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qpqq (vec : in     Vkm_GenType) return Vkm_GenType renames wzww;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqss (vec : in     Vkm_GenType) return Vkm_GenType renames wwxx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqst (vec : in     Vkm_GenType) return Vkm_GenType renames wwxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqsp (vec : in     Vkm_GenType) return Vkm_GenType renames wwxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqsq (vec : in     Vkm_GenType) return Vkm_GenType renames wwxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqts (vec : in     Vkm_GenType) return Vkm_GenType renames wwyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqtt (vec : in     Vkm_GenType) return Vkm_GenType renames wwyy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqtp (vec : in     Vkm_GenType) return Vkm_GenType renames wwyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqtq (vec : in     Vkm_GenType) return Vkm_GenType renames wwyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqps (vec : in     Vkm_GenType) return Vkm_GenType renames wwzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqpt (vec : in     Vkm_GenType) return Vkm_GenType renames wwzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqpp (vec : in     Vkm_GenType) return Vkm_GenType renames wwzz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqpq (vec : in     Vkm_GenType) return Vkm_GenType renames wwzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqqs (vec : in     Vkm_GenType) return Vkm_GenType renames wwwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqqt (vec : in     Vkm_GenType) return Vkm_GenType renames wwwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqqp (vec : in     Vkm_GenType) return Vkm_GenType renames wwwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    --<
    --< @param vec
    --< The vector to swizzle
    ----------------------------------------------------------------------------
    function qqqq (vec : in     Vkm_GenType) return Vkm_GenType renames wwww;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure xyzw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure xywz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure xzyw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure xzwy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure xwyz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure xwzy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure yxzw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure yxwz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure yzxw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure yzwx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure ywxz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure ywzx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure zxyw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure zxwy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure zyxw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure zywx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure zwxy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure zwyx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure wxyz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure wxzy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure wyxz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure wyzx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure wzxy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure wzyx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);



    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure rgba (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure rgab (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure rbga (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure rbag (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure ragb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure rabg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure grba (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure grab (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure gbra (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure gbar (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure garb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure gabr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure brga (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure brag (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure bgra (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure bgar (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure barg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure bagr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure argb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure arbg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure agrb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure agbr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure abrg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure abgr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzyx;



    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure stpq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xyzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure stqp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xywz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure sptq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure spqt (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure sqtp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure sqpt (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure tspq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxzw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure tsqp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxwz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure tpsq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure tpqs (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzwx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure tqsp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure tqps (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywzx;



    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure pstq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxyw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure psqt (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxwy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure ptsq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zyxw;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure ptqs (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zywx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure pqst (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure pqts (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwyx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure qstp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxyz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure qspt (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure qtsp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyxz;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure qtps (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyzx;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure qpst (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzxy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    --<
    --< @param vec1
    --< The value for which to set the 4D swizzle.
    --<
    --< @param vec2
    --< The 4D swizzle value.
    ----------------------------------------------------------------------------
    procedure qpts (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzyx;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply binary function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise between two vectors.
    --<
    --<    RV := [Func(IV1.x,IV2.x) ... Func(IV1.w, IV2.w)]
    --<
    --< @param left
    --< The first input vector parameter.
    --<
    --< @param right
    --< The second input vector parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(left, right : in     Base_Type) return Base_Type;

    function Apply_Func_IV_IV_RV(left, right : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply binary function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise between a vector and a scalar.
    --<
    --<    RV := [Func(IS,IV2.x) ... Func(IS, IV2.w)]
    --<
    --< @param left
    --< The first input scalar parameter.
    --<
    --< @param right
    --< The second input vector parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(left, right : in     Base_Type) return Base_Type;

    function Apply_Func_IS_IV_RV(left  : in     Base_Type;
                                 right : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply binary function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise between a vector and a scalar.
    --<
    --<    RV := [Func(IV1.x,IS) ... Func(IV1.w, IS)]
    --<
    --< @param left
    --< The first input scalar parameter.
    --<
    --< @param right
    --< The second input vector parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(Left, Right : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IS_RV(left  : in     Vkm_GenType;
                                 right : in     Base_Type  ) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply unary function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on a vector.
    --<
    --<    RV := [Func(A.x) ... Func(A.w)]
    --<
    --< @param A
    --< The input vector parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(A : in     Base_Type) return Base_Type;
    function Apply_Func_IV_RV(A : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on a vector. This outputs a
    --< vector parameter in addition to the return value.
    --<
    --<    RV := [Func(IV1.x, OV1.x) ... Func(IV1.w, OV1.w)]
    --<
    --< @param IV1
    --< The input vector parameter.
    --<
    --< @param OV1
    --< The output vector parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1 : in     Base_Type;
                           OS1 :    out Base_Type) return Base_Type;
    function Apply_Func_IV_OV_RV(IV1 : in     Vkm_GenType;
                                 OV1 :    out Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on three input vectors.
    --<
    --<    RV := [Func(IV1.x, IV2.x, IV3.x) ... Func(IV1.w, IV2.w, IV3.w)]
    --<
    --< @param IV1
    --< The first input vector parameter.
    --<
    --< @param IV2
    --< The second input vector parameter.
    --<
    --< @param IV3
    --< The third input vector parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IV_IV_RV(IV1, IV2, IV3 : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on two input vectors and a
    --< scalar value.
    --<
    --<    RV := [Func(IV1.x, IV2.x, IS1) ... Func(IV1.w, IV2.w, IS1)]
    --<
    --< @param IV1
    --< The first input vector parameter.
    --<
    --< @param IV2
    --< The second input vector parameter.
    --<
    --< @param IS1
    --< The first scalar parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IV_IS_RV(IV1, IV2 : in     Vkm_GenType;
                                    IS1      : in     Base_Type) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on one input vectors and two
    --< scalar values.
    --<
    --<    RV := [Func(IV1.x, IS1, IS2) ... Func(IV1.w, IS1, IS2)]
    --<
    --< @param IV1
    --< The first input vector parameter.
    --<
    --< @param IS1
    --< The first input scalar parameter.
    --<
    --< @param IS2
    --< The second input scalar parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IS_IS_RV(IV1      : in     Vkm_GenType;
                                    IS1, IS2 : in     Base_Type) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on one input vectors and two
    --< scalar values.
    --<
    --<    RV := [Func(IS1, IS2, IV1.x) ... Func(IS1, IS2, IV1.w)]
    --<
    --< @param IS1
    --< The first input scalar parameter.
    --<
    --< @param IS2
    --< The second input scalar parameter.
    --<
    --< @param IV1
    --< The input vector parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IS_IS_IV_RV(IS1, IS2 : in     Base_Type;
                                    IV1      : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on two input vectors and an
    --< output vector:
    --<    RV := [Func(IV1.x, IV2.x, OV1.x) ... Func(IV1.w, IV2.w, OV1.w)]
    --<
    --< @param IV1
    --< The first input vector parameter.
    --<
    --< @param IV2
    --< The second input vector parameter.
    --<
    --< @param OV1
    --< The output vector parameter.
    --<
    --< @return
    --< The result, RV.
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2 : in     Base_Type;
                           OS1      :    out Base_Type) return Base_Type;
    function Apply_Func_IV_IV_OV_RV(IV1, IV2 : in     Vkm_GenType;
                                    OV1      :    out Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on two input vectors and two
    --< output vectors:
    --<    [Func(IV1.x, IV2.x, OV1.x, OV2.x) ... Func(IV1.w, IV2.w, OV1.w, OV2.w)]
    --<
    --< @param IV1
    --< The first input vector parameter.
    --<
    --< @param IV2
    --< The second input vector parameter.
    --<
    --< @param OV1
    --< The first output vector parameter.
    --<
    --< @param OV2
    --< The second output vector parameter.
    --<
    ----------------------------------------------------------------------------
    generic
        with procedure Func(IS1, IS2 : in     Base_Type;
                            OS1, OS2 :    out Base_Type);
    procedure Apply_Func_IV_IV_OV_OV(IV1, IV2 : in     Vkm_GenType;
                                     OV1, OV2 :    out Vkm_GenType);


    ----------------------------------------------------------------------------
    --< @summary
    --< Apply function for parameters of Base_Type on Vkm_GenType vector.
    --<
    --< @description
    --< Apply's a supplied function component wise on two input vectors and two
    --< input scalars:
    --<     RV := [Func(IV1.x, IV2.x, IS1, IS2) ... Func(IV1.w, IV2.w, IS1, IS2)]
    --<
    --< @param IV1
    --< The first input vector parameter.
    --<
    --< @param IV2
    --< The second input vector parameter.
    --<
    --< @param IS1
    --< The first input scalar parameter.
    --<
    --< @param IS2
    --< The second input scalar parameter.
    --<
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2, IS3, IS4 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IV_IS_IS_RV(IV1, IV2 : in     Vkm_GenType;
                                       IS1, IS2 : in     Base_Type  ) return Vkm_GenType;


end Vulkan.Math.GenType;
