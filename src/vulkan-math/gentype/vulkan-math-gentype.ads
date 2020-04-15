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
    with function Image (Instance : in     Base_Type) return String;
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
        (vec.data(0)) with Inline;
        
        
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
        (vec.data(1)) with Inline;
        
        
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
        (vec.data(2)) with Inline;
        
        
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
        (vec.data(3)) with Inline;
        
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
    ----------------------------------------------------------------------------
    function xx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function xy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function xz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function xw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function yx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function yy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function yz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function yw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function zx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function zy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function zz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function zw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function wx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function wy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function wz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function rr (vec : in     Vkm_GenType) return Vkm_GenType renames xx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function rg (vec : in     Vkm_GenType) return Vkm_GenType renames xy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function rb (vec : in     Vkm_GenType) return Vkm_GenType renames xz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ra (vec : in     Vkm_GenType) return Vkm_GenType renames xw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function gr (vec : in     Vkm_GenType) return Vkm_GenType renames yx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function gg (vec : in     Vkm_GenType) return Vkm_GenType renames yy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function gb (vec : in     Vkm_GenType) return Vkm_GenType renames yz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ga (vec : in     Vkm_GenType) return Vkm_GenType renames yw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function br (vec : in     Vkm_GenType) return Vkm_GenType renames zx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function bg (vec : in     Vkm_GenType) return Vkm_GenType renames zy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function bb (vec : in     Vkm_GenType) return Vkm_GenType renames zz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ba (vec : in     Vkm_GenType) return Vkm_GenType renames zw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ar (vec : in     Vkm_GenType) return Vkm_GenType renames wx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ag (vec : in     Vkm_GenType) return Vkm_GenType renames wy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ab (vec : in     Vkm_GenType) return Vkm_GenType renames wz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function aa (vec : in     Vkm_GenType) return Vkm_GenType renames ww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ss (vec : in     Vkm_GenType) return Vkm_GenType renames xx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function st (vec : in     Vkm_GenType) return Vkm_GenType renames xy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function sp (vec : in     Vkm_GenType) return Vkm_GenType renames xz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function sq (vec : in     Vkm_GenType) return Vkm_GenType renames xw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ts (vec : in     Vkm_GenType) return Vkm_GenType renames yx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function tt (vec : in     Vkm_GenType) return Vkm_GenType renames yy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function tp (vec : in     Vkm_GenType) return Vkm_GenType renames yz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function tq (vec : in     Vkm_GenType) return Vkm_GenType renames yw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function ps (vec : in     Vkm_GenType) return Vkm_GenType renames zx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function pt (vec : in     Vkm_GenType) return Vkm_GenType renames zy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function pp (vec : in     Vkm_GenType) return Vkm_GenType renames zz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function pq (vec : in     Vkm_GenType) return Vkm_GenType renames zw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function qs (vec : in     Vkm_GenType) return Vkm_GenType renames wx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function qt (vec : in     Vkm_GenType) return Vkm_GenType renames wy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function qp (vec : in     Vkm_GenType) return Vkm_GenType renames wz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 2D swizzle of the instance of Vkm_GenType.
    ----------------------------------------------------------------------------
    function qq (vec : in     Vkm_GenType) return Vkm_GenType renames ww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure xy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure xz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure xw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure yx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure yz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure yw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure zx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure zy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure zw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure wx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure wy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure wz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
                  
    
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure rg (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure rb (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure ra (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure gr (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure gb (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure ga (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure br (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure bg (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure ba (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure ar (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure ag (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure ab (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wz;
                  
    
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure st (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure sp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure sq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure ts (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure tp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure tq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure ps (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure pt (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure pq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure qs (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure qt (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 2D input vec2 for ND vector vec1.
    ----------------------------------------------------------------------------
    procedure qp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wz;
        
    ----------------------------------------------------------------------------
    -- 3 D
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function www (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.w, vec.w, vec.w)) with Inline;
        

    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrr (vec : in     Vkm_GenType) return Vkm_GenType renames xxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrg (vec : in     Vkm_GenType) return Vkm_GenType renames xxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrb (vec : in     Vkm_GenType) return Vkm_GenType renames xxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rra (vec : in     Vkm_GenType) return Vkm_GenType renames xxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgr (vec : in     Vkm_GenType) return Vkm_GenType renames xyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgg (vec : in     Vkm_GenType) return Vkm_GenType renames xyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgb (vec : in     Vkm_GenType) return Vkm_GenType renames xyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rga (vec : in     Vkm_GenType) return Vkm_GenType renames xyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbr (vec : in     Vkm_GenType) return Vkm_GenType renames xzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbg (vec : in     Vkm_GenType) return Vkm_GenType renames xzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbb (vec : in     Vkm_GenType) return Vkm_GenType renames xzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rba (vec : in     Vkm_GenType) return Vkm_GenType renames xzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rar (vec : in     Vkm_GenType) return Vkm_GenType renames xwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rag (vec : in     Vkm_GenType) return Vkm_GenType renames xwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rab (vec : in     Vkm_GenType) return Vkm_GenType renames xwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function raa (vec : in     Vkm_GenType) return Vkm_GenType renames xww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grr (vec : in     Vkm_GenType) return Vkm_GenType renames yxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grg (vec : in     Vkm_GenType) return Vkm_GenType renames yxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grb (vec : in     Vkm_GenType) return Vkm_GenType renames yxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gra (vec : in     Vkm_GenType) return Vkm_GenType renames yxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggr (vec : in     Vkm_GenType) return Vkm_GenType renames yyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggg (vec : in     Vkm_GenType) return Vkm_GenType renames yyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggb (vec : in     Vkm_GenType) return Vkm_GenType renames yyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gga (vec : in     Vkm_GenType) return Vkm_GenType renames yyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbr (vec : in     Vkm_GenType) return Vkm_GenType renames yzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbg (vec : in     Vkm_GenType) return Vkm_GenType renames yzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbb (vec : in     Vkm_GenType) return Vkm_GenType renames yzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gba (vec : in     Vkm_GenType) return Vkm_GenType renames yzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gar (vec : in     Vkm_GenType) return Vkm_GenType renames ywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gag (vec : in     Vkm_GenType) return Vkm_GenType renames ywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gab (vec : in     Vkm_GenType) return Vkm_GenType renames ywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gaa (vec : in     Vkm_GenType) return Vkm_GenType renames yww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brr (vec : in     Vkm_GenType) return Vkm_GenType renames zxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brg (vec : in     Vkm_GenType) return Vkm_GenType renames zxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brb (vec : in     Vkm_GenType) return Vkm_GenType renames zxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bra (vec : in     Vkm_GenType) return Vkm_GenType renames zxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgr (vec : in     Vkm_GenType) return Vkm_GenType renames zyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgg (vec : in     Vkm_GenType) return Vkm_GenType renames zyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgb (vec : in     Vkm_GenType) return Vkm_GenType renames zyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bga (vec : in     Vkm_GenType) return Vkm_GenType renames zyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbr (vec : in     Vkm_GenType) return Vkm_GenType renames zzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbg (vec : in     Vkm_GenType) return Vkm_GenType renames zzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbb (vec : in     Vkm_GenType) return Vkm_GenType renames zzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bba (vec : in     Vkm_GenType) return Vkm_GenType renames zzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bar (vec : in     Vkm_GenType) return Vkm_GenType renames zwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bag (vec : in     Vkm_GenType) return Vkm_GenType renames zwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bab (vec : in     Vkm_GenType) return Vkm_GenType renames zwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function baa (vec : in     Vkm_GenType) return Vkm_GenType renames zww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arr (vec : in     Vkm_GenType) return Vkm_GenType renames wxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arg (vec : in     Vkm_GenType) return Vkm_GenType renames wxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arb (vec : in     Vkm_GenType) return Vkm_GenType renames wxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ara (vec : in     Vkm_GenType) return Vkm_GenType renames wxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agr (vec : in     Vkm_GenType) return Vkm_GenType renames wyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agg (vec : in     Vkm_GenType) return Vkm_GenType renames wyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agb (vec : in     Vkm_GenType) return Vkm_GenType renames wyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aga (vec : in     Vkm_GenType) return Vkm_GenType renames wyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abr (vec : in     Vkm_GenType) return Vkm_GenType renames wzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abg (vec : in     Vkm_GenType) return Vkm_GenType renames wzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abb (vec : in     Vkm_GenType) return Vkm_GenType renames wzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aba (vec : in     Vkm_GenType) return Vkm_GenType renames wzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aar (vec : in     Vkm_GenType) return Vkm_GenType renames wwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aag (vec : in     Vkm_GenType) return Vkm_GenType renames wwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aab (vec : in     Vkm_GenType) return Vkm_GenType renames wwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aaa (vec : in     Vkm_GenType) return Vkm_GenType renames www;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sss (vec : in     Vkm_GenType) return Vkm_GenType renames xxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sst (vec : in     Vkm_GenType) return Vkm_GenType renames xxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssp (vec : in     Vkm_GenType) return Vkm_GenType renames xxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssq (vec : in     Vkm_GenType) return Vkm_GenType renames xxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sts (vec : in     Vkm_GenType) return Vkm_GenType renames xyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stt (vec : in     Vkm_GenType) return Vkm_GenType renames xyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stp (vec : in     Vkm_GenType) return Vkm_GenType renames xyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stq (vec : in     Vkm_GenType) return Vkm_GenType renames xyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sps (vec : in     Vkm_GenType) return Vkm_GenType renames xzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spt (vec : in     Vkm_GenType) return Vkm_GenType renames xzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spp (vec : in     Vkm_GenType) return Vkm_GenType renames xzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spq (vec : in     Vkm_GenType) return Vkm_GenType renames xzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqs (vec : in     Vkm_GenType) return Vkm_GenType renames xwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqt (vec : in     Vkm_GenType) return Vkm_GenType renames xwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqp (vec : in     Vkm_GenType) return Vkm_GenType renames xwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqq (vec : in     Vkm_GenType) return Vkm_GenType renames xww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tss (vec : in     Vkm_GenType) return Vkm_GenType renames yxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tst (vec : in     Vkm_GenType) return Vkm_GenType renames yxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsp (vec : in     Vkm_GenType) return Vkm_GenType renames yxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsq (vec : in     Vkm_GenType) return Vkm_GenType renames yxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tts (vec : in     Vkm_GenType) return Vkm_GenType renames yyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttt (vec : in     Vkm_GenType) return Vkm_GenType renames yyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttp (vec : in     Vkm_GenType) return Vkm_GenType renames yyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttq (vec : in     Vkm_GenType) return Vkm_GenType renames yyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tps (vec : in     Vkm_GenType) return Vkm_GenType renames yzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpt (vec : in     Vkm_GenType) return Vkm_GenType renames yzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpp (vec : in     Vkm_GenType) return Vkm_GenType renames yzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpq (vec : in     Vkm_GenType) return Vkm_GenType renames yzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqs (vec : in     Vkm_GenType) return Vkm_GenType renames ywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqt (vec : in     Vkm_GenType) return Vkm_GenType renames ywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqp (vec : in     Vkm_GenType) return Vkm_GenType renames ywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqq (vec : in     Vkm_GenType) return Vkm_GenType renames yww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pss (vec : in     Vkm_GenType) return Vkm_GenType renames zxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pst (vec : in     Vkm_GenType) return Vkm_GenType renames zxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psp (vec : in     Vkm_GenType) return Vkm_GenType renames zxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psq (vec : in     Vkm_GenType) return Vkm_GenType renames zxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pts (vec : in     Vkm_GenType) return Vkm_GenType renames zyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptt (vec : in     Vkm_GenType) return Vkm_GenType renames zyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptp (vec : in     Vkm_GenType) return Vkm_GenType renames zyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptq (vec : in     Vkm_GenType) return Vkm_GenType renames zyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pps (vec : in     Vkm_GenType) return Vkm_GenType renames zzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppt (vec : in     Vkm_GenType) return Vkm_GenType renames zzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppp (vec : in     Vkm_GenType) return Vkm_GenType renames zzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppq (vec : in     Vkm_GenType) return Vkm_GenType renames zzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqs (vec : in     Vkm_GenType) return Vkm_GenType renames zwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqt (vec : in     Vkm_GenType) return Vkm_GenType renames zwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqp (vec : in     Vkm_GenType) return Vkm_GenType renames zwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqq (vec : in     Vkm_GenType) return Vkm_GenType renames zww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qss (vec : in     Vkm_GenType) return Vkm_GenType renames wxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qst (vec : in     Vkm_GenType) return Vkm_GenType renames wxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsp (vec : in     Vkm_GenType) return Vkm_GenType renames wxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsq (vec : in     Vkm_GenType) return Vkm_GenType renames wxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qts (vec : in     Vkm_GenType) return Vkm_GenType renames wyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtt (vec : in     Vkm_GenType) return Vkm_GenType renames wyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtp (vec : in     Vkm_GenType) return Vkm_GenType renames wyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtq (vec : in     Vkm_GenType) return Vkm_GenType renames wyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qps (vec : in     Vkm_GenType) return Vkm_GenType renames wzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpt (vec : in     Vkm_GenType) return Vkm_GenType renames wzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpp (vec : in     Vkm_GenType) return Vkm_GenType renames wzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpq (vec : in     Vkm_GenType) return Vkm_GenType renames wzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqs (vec : in     Vkm_GenType) return Vkm_GenType renames wwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqt (vec : in     Vkm_GenType) return Vkm_GenType renames wwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqp (vec : in     Vkm_GenType) return Vkm_GenType renames wwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 3D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqq (vec : in     Vkm_GenType) return Vkm_GenType renames www;
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xyz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xyw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xzy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xzw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xwy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xwz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure yxz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure yxw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure yzx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure yzw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure ywx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure ywz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zxy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zxw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zyx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zyw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zwx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zwy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wxy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wxz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wyx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wyz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wzx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wzy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rgb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rga (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rbg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rba (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rag (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rab (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure grb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure gra (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure gbr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure gba (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure gar (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure gab (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure brg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure bra (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure bgr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure bga (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure bar (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure bag (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure arg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure arb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure agr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure agb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure abr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure abg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzy;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure stp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure stq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure spt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure spq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure sqt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure sqp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tsp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tsq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tps (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tpq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tqs (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tqp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure pst (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure psq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure pts (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure ptq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure pqs (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure pqt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qst (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qsp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qts (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qtp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qps (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 3D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qpt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzy;
                   
    ----------------------------------------------------------------------------
    -- 4 D
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.x, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.y, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.z, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function xwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.x, vec.w, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function yzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ywww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.x, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.y, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.z, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function zwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.z, vec.w, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.x, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.y, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.z, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.x, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.y, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.z, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.x)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.y)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.z)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function wwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make_GenType(vec.y, vec.w, vec.w, vec.w)) with Inline;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrrr (vec : in     Vkm_GenType) return Vkm_GenType renames xxxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrrg (vec : in     Vkm_GenType) return Vkm_GenType renames xxxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrrb (vec : in     Vkm_GenType) return Vkm_GenType renames xxxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrra (vec : in     Vkm_GenType) return Vkm_GenType renames xxxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrgr (vec : in     Vkm_GenType) return Vkm_GenType renames xxyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrgg (vec : in     Vkm_GenType) return Vkm_GenType renames xxyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrgb (vec : in     Vkm_GenType) return Vkm_GenType renames xxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrga (vec : in     Vkm_GenType) return Vkm_GenType renames xxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrbr (vec : in     Vkm_GenType) return Vkm_GenType renames xxzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrbg (vec : in     Vkm_GenType) return Vkm_GenType renames xxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrbb (vec : in     Vkm_GenType) return Vkm_GenType renames xxzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrba (vec : in     Vkm_GenType) return Vkm_GenType renames xxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrar (vec : in     Vkm_GenType) return Vkm_GenType renames xxwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrag (vec : in     Vkm_GenType) return Vkm_GenType renames xxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rrab (vec : in     Vkm_GenType) return Vkm_GenType renames xxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rraa (vec : in     Vkm_GenType) return Vkm_GenType renames xxww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgrr (vec : in     Vkm_GenType) return Vkm_GenType renames xyxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgrg (vec : in     Vkm_GenType) return Vkm_GenType renames xyxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgrb (vec : in     Vkm_GenType) return Vkm_GenType renames xyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgra (vec : in     Vkm_GenType) return Vkm_GenType renames xyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rggr (vec : in     Vkm_GenType) return Vkm_GenType renames xyyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rggg (vec : in     Vkm_GenType) return Vkm_GenType renames xyyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rggb (vec : in     Vkm_GenType) return Vkm_GenType renames xyyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgga (vec : in     Vkm_GenType) return Vkm_GenType renames xyyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgbr (vec : in     Vkm_GenType) return Vkm_GenType renames xyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgbg (vec : in     Vkm_GenType) return Vkm_GenType renames xyzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgbb (vec : in     Vkm_GenType) return Vkm_GenType renames xyzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgba (vec : in     Vkm_GenType) return Vkm_GenType renames xyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgar (vec : in     Vkm_GenType) return Vkm_GenType renames xywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgag (vec : in     Vkm_GenType) return Vkm_GenType renames xywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgab (vec : in     Vkm_GenType) return Vkm_GenType renames xywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rgaa (vec : in     Vkm_GenType) return Vkm_GenType renames xyww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbrr (vec : in     Vkm_GenType) return Vkm_GenType renames xzxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbrg (vec : in     Vkm_GenType) return Vkm_GenType renames xzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbrb (vec : in     Vkm_GenType) return Vkm_GenType renames xzxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbra (vec : in     Vkm_GenType) return Vkm_GenType renames xzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbgr (vec : in     Vkm_GenType) return Vkm_GenType renames xzyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbgg (vec : in     Vkm_GenType) return Vkm_GenType renames xzyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbgb (vec : in     Vkm_GenType) return Vkm_GenType renames xzyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbga (vec : in     Vkm_GenType) return Vkm_GenType renames xzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbbr (vec : in     Vkm_GenType) return Vkm_GenType renames xzzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbbg (vec : in     Vkm_GenType) return Vkm_GenType renames xzzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbbb (vec : in     Vkm_GenType) return Vkm_GenType renames xzzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbba (vec : in     Vkm_GenType) return Vkm_GenType renames xzzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbar (vec : in     Vkm_GenType) return Vkm_GenType renames xzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbag (vec : in     Vkm_GenType) return Vkm_GenType renames xzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbab (vec : in     Vkm_GenType) return Vkm_GenType renames xzwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rbaa (vec : in     Vkm_GenType) return Vkm_GenType renames xzww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rarr (vec : in     Vkm_GenType) return Vkm_GenType renames xwxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rarg (vec : in     Vkm_GenType) return Vkm_GenType renames xwxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rarb (vec : in     Vkm_GenType) return Vkm_GenType renames xwxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rara (vec : in     Vkm_GenType) return Vkm_GenType renames xwxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ragr (vec : in     Vkm_GenType) return Vkm_GenType renames xwyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ragg (vec : in     Vkm_GenType) return Vkm_GenType renames xwyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ragb (vec : in     Vkm_GenType) return Vkm_GenType renames xwyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function raga (vec : in     Vkm_GenType) return Vkm_GenType renames xwyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rabr (vec : in     Vkm_GenType) return Vkm_GenType renames xwzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rabg (vec : in     Vkm_GenType) return Vkm_GenType renames xwzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function rabb (vec : in     Vkm_GenType) return Vkm_GenType renames xwzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function raba (vec : in     Vkm_GenType) return Vkm_GenType renames xwzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function raar (vec : in     Vkm_GenType) return Vkm_GenType renames xwwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function raag (vec : in     Vkm_GenType) return Vkm_GenType renames xwwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function raab (vec : in     Vkm_GenType) return Vkm_GenType renames xwwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function raaa (vec : in     Vkm_GenType) return Vkm_GenType renames xwww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grrr (vec : in     Vkm_GenType) return Vkm_GenType renames yxxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grrg (vec : in     Vkm_GenType) return Vkm_GenType renames yxxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grrb (vec : in     Vkm_GenType) return Vkm_GenType renames yxxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grra (vec : in     Vkm_GenType) return Vkm_GenType renames yxxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grgr (vec : in     Vkm_GenType) return Vkm_GenType renames yxyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grgg (vec : in     Vkm_GenType) return Vkm_GenType renames yxyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grgb (vec : in     Vkm_GenType) return Vkm_GenType renames yxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grga (vec : in     Vkm_GenType) return Vkm_GenType renames yxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grbr (vec : in     Vkm_GenType) return Vkm_GenType renames yxzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grbg (vec : in     Vkm_GenType) return Vkm_GenType renames yxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grbb (vec : in     Vkm_GenType) return Vkm_GenType renames yxzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grba (vec : in     Vkm_GenType) return Vkm_GenType renames yxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grar (vec : in     Vkm_GenType) return Vkm_GenType renames yxwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grag (vec : in     Vkm_GenType) return Vkm_GenType renames yxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function grab (vec : in     Vkm_GenType) return Vkm_GenType renames yxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function graa (vec : in     Vkm_GenType) return Vkm_GenType renames yxww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggrr (vec : in     Vkm_GenType) return Vkm_GenType renames yyxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggrg (vec : in     Vkm_GenType) return Vkm_GenType renames yyxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggrb (vec : in     Vkm_GenType) return Vkm_GenType renames yyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggra (vec : in     Vkm_GenType) return Vkm_GenType renames yyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gggr (vec : in     Vkm_GenType) return Vkm_GenType renames yyyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gggg (vec : in     Vkm_GenType) return Vkm_GenType renames yyyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gggb (vec : in     Vkm_GenType) return Vkm_GenType renames yyyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggga (vec : in     Vkm_GenType) return Vkm_GenType renames yyyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggbr (vec : in     Vkm_GenType) return Vkm_GenType renames yyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggbg (vec : in     Vkm_GenType) return Vkm_GenType renames yyzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggbb (vec : in     Vkm_GenType) return Vkm_GenType renames yyzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggba (vec : in     Vkm_GenType) return Vkm_GenType renames yyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggar (vec : in     Vkm_GenType) return Vkm_GenType renames yywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggag (vec : in     Vkm_GenType) return Vkm_GenType renames yywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggab (vec : in     Vkm_GenType) return Vkm_GenType renames yywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ggaa (vec : in     Vkm_GenType) return Vkm_GenType renames yyww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbrr (vec : in     Vkm_GenType) return Vkm_GenType renames yzxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbrg (vec : in     Vkm_GenType) return Vkm_GenType renames yzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbrb (vec : in     Vkm_GenType) return Vkm_GenType renames yzxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbra (vec : in     Vkm_GenType) return Vkm_GenType renames yzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbgr (vec : in     Vkm_GenType) return Vkm_GenType renames yzyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbgg (vec : in     Vkm_GenType) return Vkm_GenType renames yzyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbgb (vec : in     Vkm_GenType) return Vkm_GenType renames yzyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbga (vec : in     Vkm_GenType) return Vkm_GenType renames yzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbbr (vec : in     Vkm_GenType) return Vkm_GenType renames yzzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbbg (vec : in     Vkm_GenType) return Vkm_GenType renames yzzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbbb (vec : in     Vkm_GenType) return Vkm_GenType renames yzzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbba (vec : in     Vkm_GenType) return Vkm_GenType renames yzzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbar (vec : in     Vkm_GenType) return Vkm_GenType renames yzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbag (vec : in     Vkm_GenType) return Vkm_GenType renames yzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbab (vec : in     Vkm_GenType) return Vkm_GenType renames yzwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gbaa (vec : in     Vkm_GenType) return Vkm_GenType renames yzww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function garr (vec : in     Vkm_GenType) return Vkm_GenType renames ywxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function garg (vec : in     Vkm_GenType) return Vkm_GenType renames ywxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function garb (vec : in     Vkm_GenType) return Vkm_GenType renames ywxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gara (vec : in     Vkm_GenType) return Vkm_GenType renames ywxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gagr (vec : in     Vkm_GenType) return Vkm_GenType renames ywyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gagg (vec : in     Vkm_GenType) return Vkm_GenType renames ywyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gagb (vec : in     Vkm_GenType) return Vkm_GenType renames ywyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gaga (vec : in     Vkm_GenType) return Vkm_GenType renames ywyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gabr (vec : in     Vkm_GenType) return Vkm_GenType renames ywzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gabg (vec : in     Vkm_GenType) return Vkm_GenType renames ywzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gabb (vec : in     Vkm_GenType) return Vkm_GenType renames ywzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gaba (vec : in     Vkm_GenType) return Vkm_GenType renames ywzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gaar (vec : in     Vkm_GenType) return Vkm_GenType renames ywwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gaag (vec : in     Vkm_GenType) return Vkm_GenType renames ywwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gaab (vec : in     Vkm_GenType) return Vkm_GenType renames ywwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function gaaa (vec : in     Vkm_GenType) return Vkm_GenType renames ywww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brrr (vec : in     Vkm_GenType) return Vkm_GenType renames zxxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brrg (vec : in     Vkm_GenType) return Vkm_GenType renames zxxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brrb (vec : in     Vkm_GenType) return Vkm_GenType renames zxxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brra (vec : in     Vkm_GenType) return Vkm_GenType renames zxxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brgr (vec : in     Vkm_GenType) return Vkm_GenType renames zxyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brgg (vec : in     Vkm_GenType) return Vkm_GenType renames zxyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brgb (vec : in     Vkm_GenType) return Vkm_GenType renames zxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brga (vec : in     Vkm_GenType) return Vkm_GenType renames zxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brbr (vec : in     Vkm_GenType) return Vkm_GenType renames zxzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brbg (vec : in     Vkm_GenType) return Vkm_GenType renames zxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brbb (vec : in     Vkm_GenType) return Vkm_GenType renames zxzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brba (vec : in     Vkm_GenType) return Vkm_GenType renames zxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brar (vec : in     Vkm_GenType) return Vkm_GenType renames zxwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brag (vec : in     Vkm_GenType) return Vkm_GenType renames zxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function brab (vec : in     Vkm_GenType) return Vkm_GenType renames zxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function braa (vec : in     Vkm_GenType) return Vkm_GenType renames zxww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgrr (vec : in     Vkm_GenType) return Vkm_GenType renames zyxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgrg (vec : in     Vkm_GenType) return Vkm_GenType renames zyxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgrb (vec : in     Vkm_GenType) return Vkm_GenType renames zyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgra (vec : in     Vkm_GenType) return Vkm_GenType renames zyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bggr (vec : in     Vkm_GenType) return Vkm_GenType renames zyyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bggg (vec : in     Vkm_GenType) return Vkm_GenType renames zyyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bggb (vec : in     Vkm_GenType) return Vkm_GenType renames zyyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgga (vec : in     Vkm_GenType) return Vkm_GenType renames zyyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgbr (vec : in     Vkm_GenType) return Vkm_GenType renames zyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgbg (vec : in     Vkm_GenType) return Vkm_GenType renames zyzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgbb (vec : in     Vkm_GenType) return Vkm_GenType renames zyzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgba (vec : in     Vkm_GenType) return Vkm_GenType renames zyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgar (vec : in     Vkm_GenType) return Vkm_GenType renames zywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgag (vec : in     Vkm_GenType) return Vkm_GenType renames zywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgab (vec : in     Vkm_GenType) return Vkm_GenType renames zywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bgaa (vec : in     Vkm_GenType) return Vkm_GenType renames zyww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbrr (vec : in     Vkm_GenType) return Vkm_GenType renames zzxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbrg (vec : in     Vkm_GenType) return Vkm_GenType renames zzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbrb (vec : in     Vkm_GenType) return Vkm_GenType renames zzxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbra (vec : in     Vkm_GenType) return Vkm_GenType renames zzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbgr (vec : in     Vkm_GenType) return Vkm_GenType renames zzyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbgg (vec : in     Vkm_GenType) return Vkm_GenType renames zzyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbgb (vec : in     Vkm_GenType) return Vkm_GenType renames zzyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbga (vec : in     Vkm_GenType) return Vkm_GenType renames zzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbbr (vec : in     Vkm_GenType) return Vkm_GenType renames zzzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbbg (vec : in     Vkm_GenType) return Vkm_GenType renames zzzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbbb (vec : in     Vkm_GenType) return Vkm_GenType renames zzzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbba (vec : in     Vkm_GenType) return Vkm_GenType renames zzzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbar (vec : in     Vkm_GenType) return Vkm_GenType renames zzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbag (vec : in     Vkm_GenType) return Vkm_GenType renames zzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbab (vec : in     Vkm_GenType) return Vkm_GenType renames zzwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bbaa (vec : in     Vkm_GenType) return Vkm_GenType renames zzww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function barr (vec : in     Vkm_GenType) return Vkm_GenType renames zwxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function barg (vec : in     Vkm_GenType) return Vkm_GenType renames zwxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function barb (vec : in     Vkm_GenType) return Vkm_GenType renames zwxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bara (vec : in     Vkm_GenType) return Vkm_GenType renames zwxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bagr (vec : in     Vkm_GenType) return Vkm_GenType renames zwyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bagg (vec : in     Vkm_GenType) return Vkm_GenType renames zwyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function bagb (vec : in     Vkm_GenType) return Vkm_GenType renames zwyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function baga (vec : in     Vkm_GenType) return Vkm_GenType renames zwyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function babr (vec : in     Vkm_GenType) return Vkm_GenType renames zwzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function babg (vec : in     Vkm_GenType) return Vkm_GenType renames zwzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function babb (vec : in     Vkm_GenType) return Vkm_GenType renames zwzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function baba (vec : in     Vkm_GenType) return Vkm_GenType renames zwzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function baar (vec : in     Vkm_GenType) return Vkm_GenType renames zwwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function baag (vec : in     Vkm_GenType) return Vkm_GenType renames zwwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function baab (vec : in     Vkm_GenType) return Vkm_GenType renames zwwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function baaa (vec : in     Vkm_GenType) return Vkm_GenType renames zwww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arrr (vec : in     Vkm_GenType) return Vkm_GenType renames wxxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arrg (vec : in     Vkm_GenType) return Vkm_GenType renames wxxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arrb (vec : in     Vkm_GenType) return Vkm_GenType renames wxxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arra (vec : in     Vkm_GenType) return Vkm_GenType renames wxxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function argr (vec : in     Vkm_GenType) return Vkm_GenType renames wxyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function argg (vec : in     Vkm_GenType) return Vkm_GenType renames wxyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function argb (vec : in     Vkm_GenType) return Vkm_GenType renames wxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arga (vec : in     Vkm_GenType) return Vkm_GenType renames wxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arbr (vec : in     Vkm_GenType) return Vkm_GenType renames wxzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arbg (vec : in     Vkm_GenType) return Vkm_GenType renames wxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arbb (vec : in     Vkm_GenType) return Vkm_GenType renames wxzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arba (vec : in     Vkm_GenType) return Vkm_GenType renames wxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arar (vec : in     Vkm_GenType) return Vkm_GenType renames wxwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arag (vec : in     Vkm_GenType) return Vkm_GenType renames wxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function arab (vec : in     Vkm_GenType) return Vkm_GenType renames wxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function araa (vec : in     Vkm_GenType) return Vkm_GenType renames wxww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agrr (vec : in     Vkm_GenType) return Vkm_GenType renames wyxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agrg (vec : in     Vkm_GenType) return Vkm_GenType renames wyxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agrb (vec : in     Vkm_GenType) return Vkm_GenType renames wyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agra (vec : in     Vkm_GenType) return Vkm_GenType renames wyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aggr (vec : in     Vkm_GenType) return Vkm_GenType renames wyyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aggg (vec : in     Vkm_GenType) return Vkm_GenType renames wyyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aggb (vec : in     Vkm_GenType) return Vkm_GenType renames wyyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agga (vec : in     Vkm_GenType) return Vkm_GenType renames wyyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agbr (vec : in     Vkm_GenType) return Vkm_GenType renames wyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agbg (vec : in     Vkm_GenType) return Vkm_GenType renames wyzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agbb (vec : in     Vkm_GenType) return Vkm_GenType renames wyzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agba (vec : in     Vkm_GenType) return Vkm_GenType renames wyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agar (vec : in     Vkm_GenType) return Vkm_GenType renames wywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agag (vec : in     Vkm_GenType) return Vkm_GenType renames wywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agab (vec : in     Vkm_GenType) return Vkm_GenType renames wywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function agaa (vec : in     Vkm_GenType) return Vkm_GenType renames wyww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abrr (vec : in     Vkm_GenType) return Vkm_GenType renames wzxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abrg (vec : in     Vkm_GenType) return Vkm_GenType renames wzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abrb (vec : in     Vkm_GenType) return Vkm_GenType renames wzxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abra (vec : in     Vkm_GenType) return Vkm_GenType renames wzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abgr (vec : in     Vkm_GenType) return Vkm_GenType renames wzyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abgg (vec : in     Vkm_GenType) return Vkm_GenType renames wzyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abgb (vec : in     Vkm_GenType) return Vkm_GenType renames wzyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abga (vec : in     Vkm_GenType) return Vkm_GenType renames wzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abbr (vec : in     Vkm_GenType) return Vkm_GenType renames wzzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abbg (vec : in     Vkm_GenType) return Vkm_GenType renames wzzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abbb (vec : in     Vkm_GenType) return Vkm_GenType renames wzzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abba (vec : in     Vkm_GenType) return Vkm_GenType renames wzzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abar (vec : in     Vkm_GenType) return Vkm_GenType renames wzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abag (vec : in     Vkm_GenType) return Vkm_GenType renames wzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abab (vec : in     Vkm_GenType) return Vkm_GenType renames wzwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function abaa (vec : in     Vkm_GenType) return Vkm_GenType renames wzww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aarr (vec : in     Vkm_GenType) return Vkm_GenType renames wwxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aarg (vec : in     Vkm_GenType) return Vkm_GenType renames wwxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aarb (vec : in     Vkm_GenType) return Vkm_GenType renames wwxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aara (vec : in     Vkm_GenType) return Vkm_GenType renames wwxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aagr (vec : in     Vkm_GenType) return Vkm_GenType renames wwyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aagg (vec : in     Vkm_GenType) return Vkm_GenType renames wwyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aagb (vec : in     Vkm_GenType) return Vkm_GenType renames wwyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aaga (vec : in     Vkm_GenType) return Vkm_GenType renames wwyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aabr (vec : in     Vkm_GenType) return Vkm_GenType renames wwzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aabg (vec : in     Vkm_GenType) return Vkm_GenType renames wwzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aabb (vec : in     Vkm_GenType) return Vkm_GenType renames wwzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aaba (vec : in     Vkm_GenType) return Vkm_GenType renames wwzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aaar (vec : in     Vkm_GenType) return Vkm_GenType renames wwwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aaag (vec : in     Vkm_GenType) return Vkm_GenType renames wwwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aaab (vec : in     Vkm_GenType) return Vkm_GenType renames wwwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function aaaa (vec : in     Vkm_GenType) return Vkm_GenType renames wwww;
        
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssss (vec : in     Vkm_GenType) return Vkm_GenType renames xxxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssst (vec : in     Vkm_GenType) return Vkm_GenType renames xxxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sssp (vec : in     Vkm_GenType) return Vkm_GenType renames xxxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sssq (vec : in     Vkm_GenType) return Vkm_GenType renames xxxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssts (vec : in     Vkm_GenType) return Vkm_GenType renames xxyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sstt (vec : in     Vkm_GenType) return Vkm_GenType renames xxyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sstp (vec : in     Vkm_GenType) return Vkm_GenType renames xxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sstq (vec : in     Vkm_GenType) return Vkm_GenType renames xxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssps (vec : in     Vkm_GenType) return Vkm_GenType renames xxzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sspt (vec : in     Vkm_GenType) return Vkm_GenType renames xxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sspp (vec : in     Vkm_GenType) return Vkm_GenType renames xxzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sspq (vec : in     Vkm_GenType) return Vkm_GenType renames xxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssqs (vec : in     Vkm_GenType) return Vkm_GenType renames xxwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssqt (vec : in     Vkm_GenType) return Vkm_GenType renames xxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssqp (vec : in     Vkm_GenType) return Vkm_GenType renames xxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ssqq (vec : in     Vkm_GenType) return Vkm_GenType renames xxww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stss (vec : in     Vkm_GenType) return Vkm_GenType renames xyxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stst (vec : in     Vkm_GenType) return Vkm_GenType renames xyxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stsp (vec : in     Vkm_GenType) return Vkm_GenType renames xyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stsq (vec : in     Vkm_GenType) return Vkm_GenType renames xyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stts (vec : in     Vkm_GenType) return Vkm_GenType renames xyyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sttt (vec : in     Vkm_GenType) return Vkm_GenType renames xyyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sttp (vec : in     Vkm_GenType) return Vkm_GenType renames xyyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sttq (vec : in     Vkm_GenType) return Vkm_GenType renames xyyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stps (vec : in     Vkm_GenType) return Vkm_GenType renames xyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stpt (vec : in     Vkm_GenType) return Vkm_GenType renames xyzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stpp (vec : in     Vkm_GenType) return Vkm_GenType renames xyzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stpq (vec : in     Vkm_GenType) return Vkm_GenType renames xyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stqs (vec : in     Vkm_GenType) return Vkm_GenType renames xywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stqt (vec : in     Vkm_GenType) return Vkm_GenType renames xywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stqp (vec : in     Vkm_GenType) return Vkm_GenType renames xywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function stqq (vec : in     Vkm_GenType) return Vkm_GenType renames xyww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spss (vec : in     Vkm_GenType) return Vkm_GenType renames xzxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spst (vec : in     Vkm_GenType) return Vkm_GenType renames xzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spsp (vec : in     Vkm_GenType) return Vkm_GenType renames xzxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spsq (vec : in     Vkm_GenType) return Vkm_GenType renames xzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spts (vec : in     Vkm_GenType) return Vkm_GenType renames xzyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sptt (vec : in     Vkm_GenType) return Vkm_GenType renames xzyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sptp (vec : in     Vkm_GenType) return Vkm_GenType renames xzyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sptq (vec : in     Vkm_GenType) return Vkm_GenType renames xzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spps (vec : in     Vkm_GenType) return Vkm_GenType renames xzzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sppt (vec : in     Vkm_GenType) return Vkm_GenType renames xzzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sppp (vec : in     Vkm_GenType) return Vkm_GenType renames xzzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sppq (vec : in     Vkm_GenType) return Vkm_GenType renames xzzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spqs (vec : in     Vkm_GenType) return Vkm_GenType renames xzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spqt (vec : in     Vkm_GenType) return Vkm_GenType renames xzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spqp (vec : in     Vkm_GenType) return Vkm_GenType renames xzwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function spqq (vec : in     Vkm_GenType) return Vkm_GenType renames xzww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqss (vec : in     Vkm_GenType) return Vkm_GenType renames xwxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqst (vec : in     Vkm_GenType) return Vkm_GenType renames xwxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqsp (vec : in     Vkm_GenType) return Vkm_GenType renames xwxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqsq (vec : in     Vkm_GenType) return Vkm_GenType renames xwxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqts (vec : in     Vkm_GenType) return Vkm_GenType renames xwyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqtt (vec : in     Vkm_GenType) return Vkm_GenType renames xwyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqtp (vec : in     Vkm_GenType) return Vkm_GenType renames xwyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqtq (vec : in     Vkm_GenType) return Vkm_GenType renames xwyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqps (vec : in     Vkm_GenType) return Vkm_GenType renames xwzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqpt (vec : in     Vkm_GenType) return Vkm_GenType renames xwzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqpp (vec : in     Vkm_GenType) return Vkm_GenType renames xwzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqpq (vec : in     Vkm_GenType) return Vkm_GenType renames xwzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqqs (vec : in     Vkm_GenType) return Vkm_GenType renames xwwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqqt (vec : in     Vkm_GenType) return Vkm_GenType renames xwwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqqp (vec : in     Vkm_GenType) return Vkm_GenType renames xwwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function sqqq (vec : in     Vkm_GenType) return Vkm_GenType renames xwww;        
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsss (vec : in     Vkm_GenType) return Vkm_GenType renames yxxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsst (vec : in     Vkm_GenType) return Vkm_GenType renames yxxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tssp (vec : in     Vkm_GenType) return Vkm_GenType renames yxxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tssq (vec : in     Vkm_GenType) return Vkm_GenType renames yxxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsts (vec : in     Vkm_GenType) return Vkm_GenType renames yxyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tstt (vec : in     Vkm_GenType) return Vkm_GenType renames yxyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tstp (vec : in     Vkm_GenType) return Vkm_GenType renames yxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tstq (vec : in     Vkm_GenType) return Vkm_GenType renames yxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsps (vec : in     Vkm_GenType) return Vkm_GenType renames yxzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tspt (vec : in     Vkm_GenType) return Vkm_GenType renames yxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tspp (vec : in     Vkm_GenType) return Vkm_GenType renames yxzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tspq (vec : in     Vkm_GenType) return Vkm_GenType renames yxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsqs (vec : in     Vkm_GenType) return Vkm_GenType renames yxwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsqt (vec : in     Vkm_GenType) return Vkm_GenType renames yxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsqp (vec : in     Vkm_GenType) return Vkm_GenType renames yxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tsqq (vec : in     Vkm_GenType) return Vkm_GenType renames yxww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttss (vec : in     Vkm_GenType) return Vkm_GenType renames yyxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttst (vec : in     Vkm_GenType) return Vkm_GenType renames yyxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttsp (vec : in     Vkm_GenType) return Vkm_GenType renames yyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttsq (vec : in     Vkm_GenType) return Vkm_GenType renames yyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttts (vec : in     Vkm_GenType) return Vkm_GenType renames yyyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tttt (vec : in     Vkm_GenType) return Vkm_GenType renames yyyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tttp (vec : in     Vkm_GenType) return Vkm_GenType renames yyyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tttq (vec : in     Vkm_GenType) return Vkm_GenType renames yyyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttps (vec : in     Vkm_GenType) return Vkm_GenType renames yyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttpt (vec : in     Vkm_GenType) return Vkm_GenType renames yyzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttpp (vec : in     Vkm_GenType) return Vkm_GenType renames yyzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttpq (vec : in     Vkm_GenType) return Vkm_GenType renames yyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttqs (vec : in     Vkm_GenType) return Vkm_GenType renames yywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttqt (vec : in     Vkm_GenType) return Vkm_GenType renames yywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttqp (vec : in     Vkm_GenType) return Vkm_GenType renames yywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ttqq (vec : in     Vkm_GenType) return Vkm_GenType renames yyww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpss (vec : in     Vkm_GenType) return Vkm_GenType renames yzxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpst (vec : in     Vkm_GenType) return Vkm_GenType renames yzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpsp (vec : in     Vkm_GenType) return Vkm_GenType renames yzxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpsq (vec : in     Vkm_GenType) return Vkm_GenType renames yzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpts (vec : in     Vkm_GenType) return Vkm_GenType renames yzyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tptt (vec : in     Vkm_GenType) return Vkm_GenType renames yzyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tptp (vec : in     Vkm_GenType) return Vkm_GenType renames yzyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tptq (vec : in     Vkm_GenType) return Vkm_GenType renames yzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpps (vec : in     Vkm_GenType) return Vkm_GenType renames yzzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tppt (vec : in     Vkm_GenType) return Vkm_GenType renames yzzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tppp (vec : in     Vkm_GenType) return Vkm_GenType renames yzzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tppq (vec : in     Vkm_GenType) return Vkm_GenType renames yzzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpqs (vec : in     Vkm_GenType) return Vkm_GenType renames yzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpqt (vec : in     Vkm_GenType) return Vkm_GenType renames yzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpqp (vec : in     Vkm_GenType) return Vkm_GenType renames yzwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tpqq (vec : in     Vkm_GenType) return Vkm_GenType renames yzww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqss (vec : in     Vkm_GenType) return Vkm_GenType renames ywxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqst (vec : in     Vkm_GenType) return Vkm_GenType renames ywxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqsp (vec : in     Vkm_GenType) return Vkm_GenType renames ywxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqsq (vec : in     Vkm_GenType) return Vkm_GenType renames ywxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqts (vec : in     Vkm_GenType) return Vkm_GenType renames ywyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqtt (vec : in     Vkm_GenType) return Vkm_GenType renames ywyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqtp (vec : in     Vkm_GenType) return Vkm_GenType renames ywyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqtq (vec : in     Vkm_GenType) return Vkm_GenType renames ywyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqps (vec : in     Vkm_GenType) return Vkm_GenType renames ywzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqpt (vec : in     Vkm_GenType) return Vkm_GenType renames ywzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqpp (vec : in     Vkm_GenType) return Vkm_GenType renames ywzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqpq (vec : in     Vkm_GenType) return Vkm_GenType renames ywzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqqs (vec : in     Vkm_GenType) return Vkm_GenType renames ywwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqqt (vec : in     Vkm_GenType) return Vkm_GenType renames ywwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqqp (vec : in     Vkm_GenType) return Vkm_GenType renames ywwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function tqqq (vec : in     Vkm_GenType) return Vkm_GenType renames ywww;        
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psss (vec : in     Vkm_GenType) return Vkm_GenType renames zxxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psst (vec : in     Vkm_GenType) return Vkm_GenType renames zxxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pssp (vec : in     Vkm_GenType) return Vkm_GenType renames zxxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pssq (vec : in     Vkm_GenType) return Vkm_GenType renames zxxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psts (vec : in     Vkm_GenType) return Vkm_GenType renames zxyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pstt (vec : in     Vkm_GenType) return Vkm_GenType renames zxyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pstp (vec : in     Vkm_GenType) return Vkm_GenType renames zxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pstq (vec : in     Vkm_GenType) return Vkm_GenType renames zxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psps (vec : in     Vkm_GenType) return Vkm_GenType renames zxzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pspt (vec : in     Vkm_GenType) return Vkm_GenType renames zxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pspp (vec : in     Vkm_GenType) return Vkm_GenType renames zxzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pspq (vec : in     Vkm_GenType) return Vkm_GenType renames zxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psqs (vec : in     Vkm_GenType) return Vkm_GenType renames zxwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psqt (vec : in     Vkm_GenType) return Vkm_GenType renames zxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psqp (vec : in     Vkm_GenType) return Vkm_GenType renames zxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function psqq (vec : in     Vkm_GenType) return Vkm_GenType renames zxww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptss (vec : in     Vkm_GenType) return Vkm_GenType renames zyxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptst (vec : in     Vkm_GenType) return Vkm_GenType renames zyxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptsp (vec : in     Vkm_GenType) return Vkm_GenType renames zyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptsq (vec : in     Vkm_GenType) return Vkm_GenType renames zyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptts (vec : in     Vkm_GenType) return Vkm_GenType renames zyyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pttt (vec : in     Vkm_GenType) return Vkm_GenType renames zyyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pttp (vec : in     Vkm_GenType) return Vkm_GenType renames zyyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pttq (vec : in     Vkm_GenType) return Vkm_GenType renames zyyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptps (vec : in     Vkm_GenType) return Vkm_GenType renames zyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptpt (vec : in     Vkm_GenType) return Vkm_GenType renames zyzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptpp (vec : in     Vkm_GenType) return Vkm_GenType renames zyzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptpq (vec : in     Vkm_GenType) return Vkm_GenType renames zyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptqs (vec : in     Vkm_GenType) return Vkm_GenType renames zywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptqt (vec : in     Vkm_GenType) return Vkm_GenType renames zywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptqp (vec : in     Vkm_GenType) return Vkm_GenType renames zywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ptqq (vec : in     Vkm_GenType) return Vkm_GenType renames zyww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppss (vec : in     Vkm_GenType) return Vkm_GenType renames zzxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppst (vec : in     Vkm_GenType) return Vkm_GenType renames zzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppsp (vec : in     Vkm_GenType) return Vkm_GenType renames zzxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppsq (vec : in     Vkm_GenType) return Vkm_GenType renames zzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppts (vec : in     Vkm_GenType) return Vkm_GenType renames zzyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pptt (vec : in     Vkm_GenType) return Vkm_GenType renames zzyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pptp (vec : in     Vkm_GenType) return Vkm_GenType renames zzyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pptq (vec : in     Vkm_GenType) return Vkm_GenType renames zzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppps (vec : in     Vkm_GenType) return Vkm_GenType renames zzzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pppt (vec : in     Vkm_GenType) return Vkm_GenType renames zzzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pppp (vec : in     Vkm_GenType) return Vkm_GenType renames zzzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pppq (vec : in     Vkm_GenType) return Vkm_GenType renames zzzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppqs (vec : in     Vkm_GenType) return Vkm_GenType renames zzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppqt (vec : in     Vkm_GenType) return Vkm_GenType renames zzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppqp (vec : in     Vkm_GenType) return Vkm_GenType renames zzwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function ppqq (vec : in     Vkm_GenType) return Vkm_GenType renames zzww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqss (vec : in     Vkm_GenType) return Vkm_GenType renames zwxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqst (vec : in     Vkm_GenType) return Vkm_GenType renames zwxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqsp (vec : in     Vkm_GenType) return Vkm_GenType renames zwxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqsq (vec : in     Vkm_GenType) return Vkm_GenType renames zwxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqts (vec : in     Vkm_GenType) return Vkm_GenType renames zwyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqtt (vec : in     Vkm_GenType) return Vkm_GenType renames zwyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqtp (vec : in     Vkm_GenType) return Vkm_GenType renames zwyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqtq (vec : in     Vkm_GenType) return Vkm_GenType renames zwyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqps (vec : in     Vkm_GenType) return Vkm_GenType renames zwzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqpt (vec : in     Vkm_GenType) return Vkm_GenType renames zwzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqpp (vec : in     Vkm_GenType) return Vkm_GenType renames zwzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqpq (vec : in     Vkm_GenType) return Vkm_GenType renames zwzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqqs (vec : in     Vkm_GenType) return Vkm_GenType renames zwwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqqt (vec : in     Vkm_GenType) return Vkm_GenType renames zwwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqqp (vec : in     Vkm_GenType) return Vkm_GenType renames zwwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function pqqq (vec : in     Vkm_GenType) return Vkm_GenType renames zwww;        
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsss (vec : in     Vkm_GenType) return Vkm_GenType renames wxxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsst (vec : in     Vkm_GenType) return Vkm_GenType renames wxxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qssp (vec : in     Vkm_GenType) return Vkm_GenType renames wxxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qssq (vec : in     Vkm_GenType) return Vkm_GenType renames wxxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsts (vec : in     Vkm_GenType) return Vkm_GenType renames wxyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qstt (vec : in     Vkm_GenType) return Vkm_GenType renames wxyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qstp (vec : in     Vkm_GenType) return Vkm_GenType renames wxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qstq (vec : in     Vkm_GenType) return Vkm_GenType renames wxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsps (vec : in     Vkm_GenType) return Vkm_GenType renames wxzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qspt (vec : in     Vkm_GenType) return Vkm_GenType renames wxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qspp (vec : in     Vkm_GenType) return Vkm_GenType renames wxzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qspq (vec : in     Vkm_GenType) return Vkm_GenType renames wxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsqs (vec : in     Vkm_GenType) return Vkm_GenType renames wxwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsqt (vec : in     Vkm_GenType) return Vkm_GenType renames wxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsqp (vec : in     Vkm_GenType) return Vkm_GenType renames wxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qsqq (vec : in     Vkm_GenType) return Vkm_GenType renames wxww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtss (vec : in     Vkm_GenType) return Vkm_GenType renames wyxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtst (vec : in     Vkm_GenType) return Vkm_GenType renames wyxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtsp (vec : in     Vkm_GenType) return Vkm_GenType renames wyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtsq (vec : in     Vkm_GenType) return Vkm_GenType renames wyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtts (vec : in     Vkm_GenType) return Vkm_GenType renames wyyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qttt (vec : in     Vkm_GenType) return Vkm_GenType renames wyyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qttp (vec : in     Vkm_GenType) return Vkm_GenType renames wyyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qttq (vec : in     Vkm_GenType) return Vkm_GenType renames wyyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtps (vec : in     Vkm_GenType) return Vkm_GenType renames wyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtpt (vec : in     Vkm_GenType) return Vkm_GenType renames wyzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtpp (vec : in     Vkm_GenType) return Vkm_GenType renames wyzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtpq (vec : in     Vkm_GenType) return Vkm_GenType renames wyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtqs (vec : in     Vkm_GenType) return Vkm_GenType renames wywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtqt (vec : in     Vkm_GenType) return Vkm_GenType renames wywy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtqp (vec : in     Vkm_GenType) return Vkm_GenType renames wywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qtqq (vec : in     Vkm_GenType) return Vkm_GenType renames wyww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpss (vec : in     Vkm_GenType) return Vkm_GenType renames wzxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpst (vec : in     Vkm_GenType) return Vkm_GenType renames wzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpsp (vec : in     Vkm_GenType) return Vkm_GenType renames wzxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpsq (vec : in     Vkm_GenType) return Vkm_GenType renames wzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpts (vec : in     Vkm_GenType) return Vkm_GenType renames wzyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qptt (vec : in     Vkm_GenType) return Vkm_GenType renames wzyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qptp (vec : in     Vkm_GenType) return Vkm_GenType renames wzyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qptq (vec : in     Vkm_GenType) return Vkm_GenType renames wzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpps (vec : in     Vkm_GenType) return Vkm_GenType renames wzzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qppt (vec : in     Vkm_GenType) return Vkm_GenType renames wzzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qppp (vec : in     Vkm_GenType) return Vkm_GenType renames wzzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qppq (vec : in     Vkm_GenType) return Vkm_GenType renames wzzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpqs (vec : in     Vkm_GenType) return Vkm_GenType renames wzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpqt (vec : in     Vkm_GenType) return Vkm_GenType renames wzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpqp (vec : in     Vkm_GenType) return Vkm_GenType renames wzwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qpqq (vec : in     Vkm_GenType) return Vkm_GenType renames wzww;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqss (vec : in     Vkm_GenType) return Vkm_GenType renames wwxx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqst (vec : in     Vkm_GenType) return Vkm_GenType renames wwxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqsp (vec : in     Vkm_GenType) return Vkm_GenType renames wwxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqsq (vec : in     Vkm_GenType) return Vkm_GenType renames wwxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqts (vec : in     Vkm_GenType) return Vkm_GenType renames wwyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqtt (vec : in     Vkm_GenType) return Vkm_GenType renames wwyy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqtp (vec : in     Vkm_GenType) return Vkm_GenType renames wwyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqtq (vec : in     Vkm_GenType) return Vkm_GenType renames wwyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqps (vec : in     Vkm_GenType) return Vkm_GenType renames wwzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqpt (vec : in     Vkm_GenType) return Vkm_GenType renames wwzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqpp (vec : in     Vkm_GenType) return Vkm_GenType renames wwzz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqpq (vec : in     Vkm_GenType) return Vkm_GenType renames wwzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqqs (vec : in     Vkm_GenType) return Vkm_GenType renames wwwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqqt (vec : in     Vkm_GenType) return Vkm_GenType renames wwwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqqp (vec : in     Vkm_GenType) return Vkm_GenType renames wwwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Gets a 4D swizzle from an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    function qqqq (vec : in     Vkm_GenType) return Vkm_GenType renames wwww;


    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xyzw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xywz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xzyw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xzwy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xwyz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure xwzy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure yxzw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure yxwz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure yzxw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure yzwx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure ywxz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure ywzx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zxyw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zxwy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zyxw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zywx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zwxy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure zwyx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wxyz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wxzy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wyxz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wyzx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wzxy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure wzyx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);

        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rgba (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rgab (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rbga (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rbag (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure ragb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure rabg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure grba (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure grab (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure gbra (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure gbar (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure garb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure gabr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure brga (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure brag (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure bgra (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure bgar (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure barg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure bagr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure argb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure arbg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure agrb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure agbr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure abrg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure abgr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzyx;
                   
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure stpq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xyzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure stqp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xywz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure sptq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure spqt (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure sqtp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure sqpt (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tspq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxzw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tsqp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxwz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tpsq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tpqs (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzwx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tqsp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure tqps (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywzx;
                   
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure pstq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxyw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure psqt (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxwy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure ptsq (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zyxw;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure ptqs (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zywx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure pqst (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure pqts (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwyx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qstp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxyz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qspt (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxzy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qtsp (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyxz;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qtps (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyzx;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
    ----------------------------------------------------------------------------
    procedure qpst (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzxy;
        
        
    ----------------------------------------------------------------------------
    --< @private
    --< Sets a swizzle of a 4D input vec2 for an N-Dimmensional vector vec1.
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


end Vulkan.Math.GenType;
