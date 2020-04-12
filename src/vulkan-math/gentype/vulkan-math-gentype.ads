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

generic
    type Base_Type is private;
    with function Image (Instance : in     Base_Type) return String;
package Vulkan.Math.GenType is
    pragma Preelaborate;
    pragma Pure;

    type Vkm_Vector is array(Vkm_Indices range <>) of aliased Base_Type;
    pragma Convention(C,Vkm_Vector);


    type Vkm_GenType(Last_Index : Vkm_Indices) is tagged
        record
            data : Vkm_Vector(Vkm_Indices'First .. Last_Index);
        end record;
   
    type Vkm_Vector_Access(Vector : not null access Vkm_GenType) is null record
        with Implicit_Dereference => Vector;
        
    ----------------------------------------------------------------------------
    -- Operations on Vkm_GenType
    ----------------------------------------------------------------------------
    -- @brief
    -- Returns the number of components in the generic vector.
    --
    -- @param[in]     A An instance of a vector.
    --
    -- @returns The number of components in the instance.
    ----------------------------------------------------------------------------
    function Length (A : in     Vkm_GenType) return Vkm_Length;

    function Image (Instance : in     Vkm_GenType) return String;

    procedure Copy (Destination : in out Vkm_GenType;
                    Source      : in     Vkm_GenType;
                    Num_Copy    : in     Vkm_Length;
                    Offset      : in     Vkm_Indices);

    function Concatenate (Left, Right : in     Vkm_GenType) return Vkm_GenType;

    ----------------------------------------------------------------------------
    -- Constructors
    ----------------------------------------------------------------------------

    function Make (Last_Index : in     Vkm_Indices;
                   value      : in     Base_Type) return Vkm_GenType;
    function Make (value1 : in Base_Type) return Vkm_GenType;
    function Make (value1, value2 : in Base_Type) return Vkm_GenType;
    function Make (value1, value2, value3 : in Base_Type) return Vkm_GenType;
    function Make (value1, value2, value3, value4 : in Base_Type) return Vkm_GenType;
    function Make (value : in     Vkm_GenType) return Vkm_GenType;
    
    ----------------------------------------------------------------------------
    -- Vector Swizzlers 
    ----------------------------------------------------------------------------
    -- 1 D
    ----------------------------------------------------------------------------
    function x (vec : in     Vkm_GenType) return Base_Type is
        (vec.data(0)) with Inline;
    function y (vec : in     Vkm_GenType) return Base_Type is
        (vec.data(1)) with Inline;
    function z (vec : in     Vkm_GenType) return Base_Type is
        (vec.data(2)) with Inline;
    function w (vec : in     Vkm_GenType) return Base_Type is
        (vec.data(3)) with Inline;
        
    function r (vec : in     Vkm_GenType) return Base_Type renames x;
    function g (vec : in     Vkm_GenType) return Base_Type renames y;
    function b (vec : in     Vkm_GenType) return Base_Type renames z;
    function a (vec : in     Vkm_GenType) return Base_Type renames w;
        
    function s (vec : in     Vkm_GenType) return Base_Type renames x;
    function t (vec : in     Vkm_GenType) return Base_Type renames y;
    function p (vec : in     Vkm_GenType) return Base_Type renames z;
    function q (vec : in     Vkm_GenType) return Base_Type renames w;
    
    -- Set while passing reference to changed instance.
    function  x (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access;          
    function  y (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access;          
    function  z (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access;         
    function  w (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access;
                 
    function  r (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access renames x;
    function  g (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access renames y;
    function  b (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access renames z;
    function  a (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access renames w;
                 
    function  s (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access renames x;
    function  t (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access renames y;
    function  p (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access renames z;
    function  q (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) return Vkm_Vector_Access renames w;
                 
    -- Set without passing reference to changed instance.
    procedure x (vec1  : in out Vkm_GenType;
                 value : in    Base_Type  );  
    procedure y (vec1  : in out Vkm_GenType;
                 value : in    Base_Type  );  
    procedure z (vec1  : in out Vkm_GenType;
                 value : in    Base_Type  );  
    procedure w (vec1  : in out Vkm_GenType;
                 value : in    Base_Type  );
                 
    procedure r (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames x;
    procedure g (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames y;
    procedure b (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames z;
    procedure a (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames w;
                 
    procedure s (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames x;
    procedure t (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames y;
    procedure p (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames z;
    procedure q (vec1  : in out Vkm_GenType;
                 value : in     Base_Type  ) renames w;
                
                
    ----------------------------------------------------------------------------
    -- 2 D
    ----------------------------------------------------------------------------
    -- Get
    function xx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x)) with Inline;
    function xy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y)) with Inline;
    function xz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z)) with Inline;
    function xw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w)) with Inline;
    function yx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x)) with Inline;
    function yy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y)) with Inline;
    function yz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z)) with Inline;
    function yw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w)) with Inline;
    function zx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x)) with Inline;
    function zy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y)) with Inline;
    function zz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z)) with Inline;
    function zw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w)) with Inline;
    function wx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.x)) with Inline;
    function wy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.y)) with Inline;
    function wz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.z)) with Inline;
    function ww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.w)) with Inline;
        
    function rr (vec : in     Vkm_GenType) return Vkm_GenType renames xx;
    function rg (vec : in     Vkm_GenType) return Vkm_GenType renames xy;
    function rb (vec : in     Vkm_GenType) return Vkm_GenType renames xz;
    function ra (vec : in     Vkm_GenType) return Vkm_GenType renames xw;
    function gr (vec : in     Vkm_GenType) return Vkm_GenType renames yx;
    function gg (vec : in     Vkm_GenType) return Vkm_GenType renames yy;
    function gb (vec : in     Vkm_GenType) return Vkm_GenType renames yz;
    function ga (vec : in     Vkm_GenType) return Vkm_GenType renames yw;
    function br (vec : in     Vkm_GenType) return Vkm_GenType renames zx;
    function bg (vec : in     Vkm_GenType) return Vkm_GenType renames zy;
    function bb (vec : in     Vkm_GenType) return Vkm_GenType renames zz;
    function ba (vec : in     Vkm_GenType) return Vkm_GenType renames zw;
    function ar (vec : in     Vkm_GenType) return Vkm_GenType renames wx;
    function ag (vec : in     Vkm_GenType) return Vkm_GenType renames wy;
    function ab (vec : in     Vkm_GenType) return Vkm_GenType renames wz;
    function aa (vec : in     Vkm_GenType) return Vkm_GenType renames ww;
        
    function ss (vec : in     Vkm_GenType) return Vkm_GenType renames xx;
    function st (vec : in     Vkm_GenType) return Vkm_GenType renames xy;
    function sp (vec : in     Vkm_GenType) return Vkm_GenType renames xz;
    function sq (vec : in     Vkm_GenType) return Vkm_GenType renames xw;
    function ts (vec : in     Vkm_GenType) return Vkm_GenType renames yx;
    function tt (vec : in     Vkm_GenType) return Vkm_GenType renames yy;
    function tp (vec : in     Vkm_GenType) return Vkm_GenType renames yz;
    function tq (vec : in     Vkm_GenType) return Vkm_GenType renames yw;
    function ps (vec : in     Vkm_GenType) return Vkm_GenType renames zx;
    function pt (vec : in     Vkm_GenType) return Vkm_GenType renames zy;
    function pp (vec : in     Vkm_GenType) return Vkm_GenType renames zz;
    function pq (vec : in     Vkm_GenType) return Vkm_GenType renames zw;
    function qs (vec : in     Vkm_GenType) return Vkm_GenType renames wx;
    function qt (vec : in     Vkm_GenType) return Vkm_GenType renames wy;
    function qp (vec : in     Vkm_GenType) return Vkm_GenType renames wz;
    function qq (vec : in     Vkm_GenType) return Vkm_GenType renames ww;
        
    -- Set
    procedure xy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure xz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure xw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure yx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure yz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure yw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure zx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure zy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure zw (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure wx (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure wy (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
    procedure wz (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType);
                  
    
    procedure rg (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xy;
    procedure rb (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xz;
    procedure ra (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xw;
    procedure gr (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yx;
    procedure gb (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yz;
    procedure ga (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yw;
    procedure br (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zx;
    procedure bg (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zy;
    procedure ba (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zw;
    procedure ar (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wx;
    procedure ag (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wy;
    procedure ab (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wz;
                  
    
    procedure st (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xy;
    procedure sp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xz;
    procedure sq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames xw;
    procedure ts (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yx;
    procedure tp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yz;
    procedure tq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames yw;
    procedure ps (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zx;
    procedure pt (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zy;
    procedure pq (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames zw;
    procedure qs (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wx;
    procedure qt (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wy;
    procedure qp (vec1 : in out Vkm_GenType;
                  vec2 : in     Vkm_GenType) renames wz;
        
    ----------------------------------------------------------------------------
    -- 3 D
    ----------------------------------------------------------------------------
    function xxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.x)) with Inline;
    function xxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.y)) with Inline;
    function xxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.z)) with Inline;
    function xxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.w)) with Inline;
    function xyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.x)) with Inline;
    function xyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.y)) with Inline;
    function xyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.z)) with Inline;
    function xyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.w)) with Inline;
    function xzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.x)) with Inline;
    function xzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.y)) with Inline;
    function xzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.z)) with Inline;
    function xzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.w)) with Inline;
    function xwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.x)) with Inline;
    function xwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.y)) with Inline;
    function xwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.z)) with Inline;
    function xww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.w)) with Inline;
    function yxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x)) with Inline;
    function yxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y)) with Inline;
    function yxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z)) with Inline;
    function yxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w)) with Inline;
    function yyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x)) with Inline;
    function yyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y)) with Inline;
    function yyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z)) with Inline;
    function yyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w)) with Inline;
    function yzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x)) with Inline;
    function yzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y)) with Inline;
    function yzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z)) with Inline;
    function yzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w)) with Inline;
    function ywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x)) with Inline;
    function ywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y)) with Inline;
    function ywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z)) with Inline;
    function yww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w)) with Inline;
    function zxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.x)) with Inline;
    function zxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.y)) with Inline;
    function zxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.z)) with Inline;
    function zxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.w)) with Inline;
    function zyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.x)) with Inline;
    function zyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.y)) with Inline;
    function zyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.z)) with Inline;
    function zyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.w)) with Inline;
    function zzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.x)) with Inline;
    function zzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.y)) with Inline;
    function zzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.z)) with Inline;
    function zzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.w)) with Inline;
    function zwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.x)) with Inline;
    function zwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.y)) with Inline;
    function zwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.z)) with Inline;
    function zww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.w)) with Inline;
    function wxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.x, vec.x)) with Inline;
    function wxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.x, vec.y)) with Inline;
    function wxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.x, vec.z)) with Inline;
    function wxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.x, vec.w)) with Inline;
    function wyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.y, vec.x)) with Inline;
    function wyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.y, vec.y)) with Inline;
    function wyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.y, vec.z)) with Inline;
    function wyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.y, vec.w)) with Inline;
    function wzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.z, vec.x)) with Inline;
    function wzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.z, vec.y)) with Inline;
    function wzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.z, vec.z)) with Inline;
    function wzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.z, vec.w)) with Inline;
    function wwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.w, vec.x)) with Inline;
    function wwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.w, vec.y)) with Inline;
    function wwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.w, vec.z)) with Inline;
    function www (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.w, vec.w, vec.w)) with Inline;
        

    function rrr (vec : in     Vkm_GenType) return Vkm_GenType renames xxx;
    function rrg (vec : in     Vkm_GenType) return Vkm_GenType renames xxy;
    function rrb (vec : in     Vkm_GenType) return Vkm_GenType renames xxz;
    function rra (vec : in     Vkm_GenType) return Vkm_GenType renames xxw;
    function rgr (vec : in     Vkm_GenType) return Vkm_GenType renames xyx;
    function rgg (vec : in     Vkm_GenType) return Vkm_GenType renames xyy;
    function rgb (vec : in     Vkm_GenType) return Vkm_GenType renames xyz;
    function rga (vec : in     Vkm_GenType) return Vkm_GenType renames xyw;
    function rbr (vec : in     Vkm_GenType) return Vkm_GenType renames xzx;
    function rbg (vec : in     Vkm_GenType) return Vkm_GenType renames xzy;
    function rbb (vec : in     Vkm_GenType) return Vkm_GenType renames xzz;
    function rba (vec : in     Vkm_GenType) return Vkm_GenType renames xzw;
    function rar (vec : in     Vkm_GenType) return Vkm_GenType renames xwx;
    function rag (vec : in     Vkm_GenType) return Vkm_GenType renames xwy;
    function rab (vec : in     Vkm_GenType) return Vkm_GenType renames xwz;
    function raa (vec : in     Vkm_GenType) return Vkm_GenType renames xww;
    function grr (vec : in     Vkm_GenType) return Vkm_GenType renames yxx;
    function grg (vec : in     Vkm_GenType) return Vkm_GenType renames yxy;
    function grb (vec : in     Vkm_GenType) return Vkm_GenType renames yxz;
    function gra (vec : in     Vkm_GenType) return Vkm_GenType renames yxw;
    function ggr (vec : in     Vkm_GenType) return Vkm_GenType renames yyx;
    function ggg (vec : in     Vkm_GenType) return Vkm_GenType renames yyy;
    function ggb (vec : in     Vkm_GenType) return Vkm_GenType renames yyz;
    function gga (vec : in     Vkm_GenType) return Vkm_GenType renames yyw;
    function gbr (vec : in     Vkm_GenType) return Vkm_GenType renames yzx;
    function gbg (vec : in     Vkm_GenType) return Vkm_GenType renames yzy;
    function gbb (vec : in     Vkm_GenType) return Vkm_GenType renames yzz;
    function gba (vec : in     Vkm_GenType) return Vkm_GenType renames yzw;
    function gar (vec : in     Vkm_GenType) return Vkm_GenType renames ywx;
    function gag (vec : in     Vkm_GenType) return Vkm_GenType renames ywy;
    function gab (vec : in     Vkm_GenType) return Vkm_GenType renames ywz;
    function gaa (vec : in     Vkm_GenType) return Vkm_GenType renames yww;
    function brr (vec : in     Vkm_GenType) return Vkm_GenType renames zxx;
    function brg (vec : in     Vkm_GenType) return Vkm_GenType renames zxy;
    function brb (vec : in     Vkm_GenType) return Vkm_GenType renames zxz;
    function bra (vec : in     Vkm_GenType) return Vkm_GenType renames zxw;
    function bgr (vec : in     Vkm_GenType) return Vkm_GenType renames zyx;
    function bgg (vec : in     Vkm_GenType) return Vkm_GenType renames zyy;
    function bgb (vec : in     Vkm_GenType) return Vkm_GenType renames zyz;
    function bga (vec : in     Vkm_GenType) return Vkm_GenType renames zyw;
    function bbr (vec : in     Vkm_GenType) return Vkm_GenType renames zzx;
    function bbg (vec : in     Vkm_GenType) return Vkm_GenType renames zzy;
    function bbb (vec : in     Vkm_GenType) return Vkm_GenType renames zzz;
    function bba (vec : in     Vkm_GenType) return Vkm_GenType renames zzw;
    function bar (vec : in     Vkm_GenType) return Vkm_GenType renames zwx;
    function bag (vec : in     Vkm_GenType) return Vkm_GenType renames zwy;
    function bab (vec : in     Vkm_GenType) return Vkm_GenType renames zwz;
    function baa (vec : in     Vkm_GenType) return Vkm_GenType renames zww;
    function arr (vec : in     Vkm_GenType) return Vkm_GenType renames wxx;
    function arg (vec : in     Vkm_GenType) return Vkm_GenType renames wxy;
    function arb (vec : in     Vkm_GenType) return Vkm_GenType renames wxz;
    function ara (vec : in     Vkm_GenType) return Vkm_GenType renames wxw;
    function agr (vec : in     Vkm_GenType) return Vkm_GenType renames wyx;
    function agg (vec : in     Vkm_GenType) return Vkm_GenType renames wyy;
    function agb (vec : in     Vkm_GenType) return Vkm_GenType renames wyz;
    function aga (vec : in     Vkm_GenType) return Vkm_GenType renames wyw;
    function abr (vec : in     Vkm_GenType) return Vkm_GenType renames wzx;
    function abg (vec : in     Vkm_GenType) return Vkm_GenType renames wzy;
    function abb (vec : in     Vkm_GenType) return Vkm_GenType renames wzz;
    function aba (vec : in     Vkm_GenType) return Vkm_GenType renames wzw;
    function aar (vec : in     Vkm_GenType) return Vkm_GenType renames wwx;
    function aag (vec : in     Vkm_GenType) return Vkm_GenType renames wwy;
    function aab (vec : in     Vkm_GenType) return Vkm_GenType renames wwz;
    function aaa (vec : in     Vkm_GenType) return Vkm_GenType renames www;
        
    function sss (vec : in     Vkm_GenType) return Vkm_GenType renames xxx;
    function sst (vec : in     Vkm_GenType) return Vkm_GenType renames xxy;
    function ssp (vec : in     Vkm_GenType) return Vkm_GenType renames xxz;
    function ssq (vec : in     Vkm_GenType) return Vkm_GenType renames xxw;
    function sts (vec : in     Vkm_GenType) return Vkm_GenType renames xyx;
    function stt (vec : in     Vkm_GenType) return Vkm_GenType renames xyy;
    function stp (vec : in     Vkm_GenType) return Vkm_GenType renames xyz;
    function stq (vec : in     Vkm_GenType) return Vkm_GenType renames xyw;
    function sps (vec : in     Vkm_GenType) return Vkm_GenType renames xzx;
    function spt (vec : in     Vkm_GenType) return Vkm_GenType renames xzy;
    function spp (vec : in     Vkm_GenType) return Vkm_GenType renames xzz;
    function spq (vec : in     Vkm_GenType) return Vkm_GenType renames xzw;
    function sqs (vec : in     Vkm_GenType) return Vkm_GenType renames xwx;
    function sqt (vec : in     Vkm_GenType) return Vkm_GenType renames xwy;
    function sqp (vec : in     Vkm_GenType) return Vkm_GenType renames xwz;
    function sqq (vec : in     Vkm_GenType) return Vkm_GenType renames xww;
    function tss (vec : in     Vkm_GenType) return Vkm_GenType renames yxx;
    function tst (vec : in     Vkm_GenType) return Vkm_GenType renames yxy;
    function tsp (vec : in     Vkm_GenType) return Vkm_GenType renames yxz;
    function tsq (vec : in     Vkm_GenType) return Vkm_GenType renames yxw;
    function tts (vec : in     Vkm_GenType) return Vkm_GenType renames yyx;
    function ttt (vec : in     Vkm_GenType) return Vkm_GenType renames yyy;
    function ttp (vec : in     Vkm_GenType) return Vkm_GenType renames yyz;
    function ttq (vec : in     Vkm_GenType) return Vkm_GenType renames yyw;
    function tps (vec : in     Vkm_GenType) return Vkm_GenType renames yzx;
    function tpt (vec : in     Vkm_GenType) return Vkm_GenType renames yzy;
    function tpp (vec : in     Vkm_GenType) return Vkm_GenType renames yzz;
    function tpq (vec : in     Vkm_GenType) return Vkm_GenType renames yzw;
    function tqs (vec : in     Vkm_GenType) return Vkm_GenType renames ywx;
    function tqt (vec : in     Vkm_GenType) return Vkm_GenType renames ywy;
    function tqp (vec : in     Vkm_GenType) return Vkm_GenType renames ywz;
    function tqq (vec : in     Vkm_GenType) return Vkm_GenType renames yww;
    function pss (vec : in     Vkm_GenType) return Vkm_GenType renames zxx;
    function pst (vec : in     Vkm_GenType) return Vkm_GenType renames zxy;
    function psp (vec : in     Vkm_GenType) return Vkm_GenType renames zxz;
    function psq (vec : in     Vkm_GenType) return Vkm_GenType renames zxw;
    function pts (vec : in     Vkm_GenType) return Vkm_GenType renames zyx;
    function ptt (vec : in     Vkm_GenType) return Vkm_GenType renames zyy;
    function ptp (vec : in     Vkm_GenType) return Vkm_GenType renames zyz;
    function ptq (vec : in     Vkm_GenType) return Vkm_GenType renames zyw;
    function pps (vec : in     Vkm_GenType) return Vkm_GenType renames zzx;
    function ppt (vec : in     Vkm_GenType) return Vkm_GenType renames zzy;
    function ppp (vec : in     Vkm_GenType) return Vkm_GenType renames zzz;
    function ppq (vec : in     Vkm_GenType) return Vkm_GenType renames zzw;
    function pqs (vec : in     Vkm_GenType) return Vkm_GenType renames zwx;
    function pqt (vec : in     Vkm_GenType) return Vkm_GenType renames zwy;
    function pqp (vec : in     Vkm_GenType) return Vkm_GenType renames zwz;
    function pqq (vec : in     Vkm_GenType) return Vkm_GenType renames zww;
    function qss (vec : in     Vkm_GenType) return Vkm_GenType renames wxx;
    function qst (vec : in     Vkm_GenType) return Vkm_GenType renames wxy;
    function qsp (vec : in     Vkm_GenType) return Vkm_GenType renames wxz;
    function qsq (vec : in     Vkm_GenType) return Vkm_GenType renames wxw;
    function qts (vec : in     Vkm_GenType) return Vkm_GenType renames wyx;
    function qtt (vec : in     Vkm_GenType) return Vkm_GenType renames wyy;
    function qtp (vec : in     Vkm_GenType) return Vkm_GenType renames wyz;
    function qtq (vec : in     Vkm_GenType) return Vkm_GenType renames wyw;
    function qps (vec : in     Vkm_GenType) return Vkm_GenType renames wzx;
    function qpt (vec : in     Vkm_GenType) return Vkm_GenType renames wzy;
    function qpp (vec : in     Vkm_GenType) return Vkm_GenType renames wzz;
    function qpq (vec : in     Vkm_GenType) return Vkm_GenType renames wzw;
    function qqs (vec : in     Vkm_GenType) return Vkm_GenType renames wwx;
    function qqt (vec : in     Vkm_GenType) return Vkm_GenType renames wwy;
    function qqp (vec : in     Vkm_GenType) return Vkm_GenType renames wwz;
    function qqq (vec : in     Vkm_GenType) return Vkm_GenType renames www;
        
        
    -- Set
    procedure xyz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure xyw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure xzy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure xzw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure xwy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure xwz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure yxz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure yxw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure yzx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure yzw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure ywx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure ywz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure zxy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure zxw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure zyx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure zyw (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure zwx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure zwy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure wxy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure wxz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure wyx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure wyz (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure wzx (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);
    procedure wzy (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType);


    procedure rgb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyz;
    procedure rga (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyw;
    procedure rbg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzy;
    procedure rba (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzw;
    procedure rag (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwy;
    procedure rab (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwz;
    procedure grb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxz;
    procedure gra (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxw;
    procedure gbr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzx;
    procedure gba (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzw;
    procedure gar (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywx;
    procedure gab (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywz;
    procedure brg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxy;
    procedure bra (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxw;
    procedure bgr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyx;
    procedure bga (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyw;
    procedure bar (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwx;
    procedure bag (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwy;
    procedure arg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxy;
    procedure arb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxz;
    procedure agr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyx;
    procedure agb (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyz;
    procedure abr (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzx;
    procedure abg (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzy;
                   
                   
    procedure stp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyz;
    procedure stq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xyw;
    procedure spt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzy;
    procedure spq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xzw;
    procedure sqt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwy;
    procedure sqp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames xwz;
    procedure tsp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxz;
    procedure tsq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yxw;
    procedure tps (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzx;
    procedure tpq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames yzw;
    procedure tqs (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywx;
    procedure tqp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames ywz;
    procedure pst (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxy;
    procedure psq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zxw;
    procedure pts (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyx;
    procedure ptq (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zyw;
    procedure pqs (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwx;
    procedure pqt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames zwy;
    procedure qst (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxy;
    procedure qsp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wxz;
    procedure qts (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyx;
    procedure qtp (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wyz;
    procedure qps (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzx;
    procedure qpt (vec1 : in out Vkm_GenType;
                   vec2 : in     Vkm_GenType) renames wzy;
                   
    ----------------------------------------------------------------------------
    -- 4 D
    ----------------------------------------------------------------------------
    function xxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.x, vec.x)) with Inline;
    function xxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.x, vec.y)) with Inline;
    function xxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.x, vec.z)) with Inline;
    function xxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.x, vec.w)) with Inline;
    function xxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.y, vec.x)) with Inline;
    function xxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.y, vec.y)) with Inline;
    function xxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.y, vec.z)) with Inline;
    function xxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.y, vec.w)) with Inline;
    function xxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.z, vec.x)) with Inline;
    function xxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.z, vec.y)) with Inline;
    function xxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.z, vec.z)) with Inline;
    function xxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.z, vec.w)) with Inline;
    function xxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.w, vec.x)) with Inline;
    function xxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.w, vec.y)) with Inline;
    function xxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.w, vec.z)) with Inline;
    function xxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.x, vec.w, vec.w)) with Inline;
    function xyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.x, vec.x)) with Inline;
    function xyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.x, vec.y)) with Inline;
    function xyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.x, vec.z)) with Inline;
    function xyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.x, vec.w)) with Inline;
    function xyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.y, vec.x)) with Inline;
    function xyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.y, vec.y)) with Inline;
    function xyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.y, vec.z)) with Inline;
    function xyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.y, vec.w)) with Inline;
    function xyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.z, vec.x)) with Inline;
    function xyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.z, vec.y)) with Inline;
    function xyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.z, vec.z)) with Inline;
    function xyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.z, vec.w)) with Inline;
    function xywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.w, vec.x)) with Inline;
    function xywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.w, vec.y)) with Inline;
    function xywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.w, vec.z)) with Inline;
    function xyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.y, vec.w, vec.w)) with Inline;
    function xzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.x, vec.x)) with Inline;
    function xzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.x, vec.y)) with Inline;
    function xzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.x, vec.z)) with Inline;
    function xzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.x, vec.w)) with Inline;
    function xzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.y, vec.x)) with Inline;
    function xzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.y, vec.y)) with Inline;
    function xzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.y, vec.z)) with Inline;
    function xzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.y, vec.w)) with Inline;
    function xzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.z, vec.x)) with Inline;
    function xzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.z, vec.y)) with Inline;
    function xzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.z, vec.z)) with Inline;
    function xzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.z, vec.w)) with Inline;
    function xzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.w, vec.x)) with Inline;
    function xzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.w, vec.y)) with Inline;
    function xzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.w, vec.z)) with Inline;
    function xzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.z, vec.w, vec.w)) with Inline;
    function xwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.x, vec.x)) with Inline;
    function xwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.x, vec.y)) with Inline;
    function xwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.x, vec.z)) with Inline;
    function xwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.x, vec.w)) with Inline;
    function xwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.y, vec.x)) with Inline;
    function xwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.y, vec.y)) with Inline;
    function xwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.y, vec.z)) with Inline;
    function xwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.y, vec.w)) with Inline;
    function xwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.z, vec.x)) with Inline;
    function xwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.z, vec.y)) with Inline;
    function xwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.z, vec.z)) with Inline;
    function xwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.z, vec.w)) with Inline;
    function xwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.w, vec.x)) with Inline;
    function xwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.w, vec.y)) with Inline;
    function xwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.w, vec.z)) with Inline;
    function xwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.x, vec.w, vec.w, vec.w)) with Inline;
    function yxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x, vec.x)) with Inline;
    function yxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x, vec.y)) with Inline;
    function yxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x, vec.z)) with Inline;
    function yxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x, vec.w)) with Inline;
    function yxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y, vec.x)) with Inline;
    function yxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y, vec.y)) with Inline;
    function yxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y, vec.z)) with Inline;
    function yxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y, vec.w)) with Inline;
    function yxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z, vec.x)) with Inline;
    function yxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z, vec.y)) with Inline;
    function yxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z, vec.z)) with Inline;
    function yxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z, vec.w)) with Inline;
    function yxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w, vec.x)) with Inline;
    function yxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w, vec.y)) with Inline;
    function yxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w, vec.z)) with Inline;
    function yxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w, vec.w)) with Inline;
    function yyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x, vec.x)) with Inline;
    function yyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x, vec.y)) with Inline;
    function yyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x, vec.z)) with Inline;
    function yyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x, vec.w)) with Inline;
    function yyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y, vec.x)) with Inline;
    function yyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y, vec.y)) with Inline;
    function yyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y, vec.z)) with Inline;
    function yyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y, vec.w)) with Inline;
    function yyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z, vec.x)) with Inline;
    function yyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z, vec.y)) with Inline;
    function yyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z, vec.z)) with Inline;
    function yyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z, vec.w)) with Inline;
    function yywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w, vec.x)) with Inline;
    function yywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w, vec.y)) with Inline;
    function yywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w, vec.z)) with Inline;
    function yyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w, vec.w)) with Inline;
    function yzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x, vec.x)) with Inline;
    function yzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x, vec.y)) with Inline;
    function yzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x, vec.z)) with Inline;
    function yzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x, vec.w)) with Inline;
    function yzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y, vec.x)) with Inline;
    function yzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y, vec.y)) with Inline;
    function yzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y, vec.z)) with Inline;
    function yzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y, vec.w)) with Inline;
    function yzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z, vec.x)) with Inline;
    function yzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z, vec.y)) with Inline;
    function yzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z, vec.z)) with Inline;
    function yzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z, vec.w)) with Inline;
    function yzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w, vec.x)) with Inline;
    function yzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w, vec.y)) with Inline;
    function yzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w, vec.z)) with Inline;
    function yzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w, vec.w)) with Inline;
    function ywxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x, vec.x)) with Inline;
    function ywxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x, vec.y)) with Inline;
    function ywxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x, vec.z)) with Inline;
    function ywxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x, vec.w)) with Inline;
    function ywyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y, vec.x)) with Inline;
    function ywyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y, vec.y)) with Inline;
    function ywyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y, vec.z)) with Inline;
    function ywyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y, vec.w)) with Inline;
    function ywzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z, vec.x)) with Inline;
    function ywzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z, vec.y)) with Inline;
    function ywzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z, vec.z)) with Inline;
    function ywzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z, vec.w)) with Inline;
    function ywwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w, vec.x)) with Inline;
    function ywwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w, vec.y)) with Inline;
    function ywwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w, vec.z)) with Inline;
    function ywww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w, vec.w)) with Inline;
    function zxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.x, vec.x)) with Inline;
    function zxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.x, vec.y)) with Inline;
    function zxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.x, vec.z)) with Inline;
    function zxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.x, vec.w)) with Inline;
    function zxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.y, vec.x)) with Inline;
    function zxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.y, vec.y)) with Inline;
    function zxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.y, vec.z)) with Inline;
    function zxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.y, vec.w)) with Inline;
    function zxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.z, vec.x)) with Inline;
    function zxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.z, vec.y)) with Inline;
    function zxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.z, vec.z)) with Inline;
    function zxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.z, vec.w)) with Inline;
    function zxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.w, vec.x)) with Inline;
    function zxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.w, vec.y)) with Inline;
    function zxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.w, vec.z)) with Inline;
    function zxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.x, vec.w, vec.w)) with Inline;
    function zyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.x, vec.x)) with Inline;
    function zyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.x, vec.y)) with Inline;
    function zyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.x, vec.z)) with Inline;
    function zyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.x, vec.w)) with Inline;
    function zyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.y, vec.x)) with Inline;
    function zyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.y, vec.y)) with Inline;
    function zyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.y, vec.z)) with Inline;
    function zyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.y, vec.w)) with Inline;
    function zyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.z, vec.x)) with Inline;
    function zyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.z, vec.y)) with Inline;
    function zyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.z, vec.z)) with Inline;
    function zyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.z, vec.w)) with Inline;
    function zywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.w, vec.x)) with Inline;
    function zywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.w, vec.y)) with Inline;
    function zywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.w, vec.z)) with Inline;
    function zyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.y, vec.w, vec.w)) with Inline;
    function zzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.x, vec.x)) with Inline;
    function zzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.x, vec.y)) with Inline;
    function zzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.x, vec.z)) with Inline;
    function zzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.x, vec.w)) with Inline;
    function zzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.y, vec.x)) with Inline;
    function zzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.y, vec.y)) with Inline;
    function zzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.y, vec.z)) with Inline;
    function zzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.y, vec.w)) with Inline;
    function zzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.z, vec.x)) with Inline;
    function zzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.z, vec.y)) with Inline;
    function zzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.z, vec.z)) with Inline;
    function zzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.z, vec.w)) with Inline;
    function zzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.w, vec.x)) with Inline;
    function zzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.w, vec.y)) with Inline;
    function zzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.w, vec.z)) with Inline;
    function zzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.z, vec.w, vec.w)) with Inline;
    function zwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.x, vec.x)) with Inline;
    function zwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.x, vec.y)) with Inline;
    function zwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.x, vec.z)) with Inline;
    function zwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.x, vec.w)) with Inline;
    function zwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.y, vec.x)) with Inline;
    function zwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.y, vec.y)) with Inline;
    function zwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.y, vec.z)) with Inline;
    function zwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.y, vec.w)) with Inline;
    function zwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.z, vec.x)) with Inline;
    function zwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.z, vec.y)) with Inline;
    function zwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.z, vec.z)) with Inline;
    function zwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.z, vec.w)) with Inline;
    function zwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.w, vec.x)) with Inline;
    function zwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.w, vec.y)) with Inline;
    function zwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.w, vec.z)) with Inline;
    function zwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.z, vec.w, vec.w, vec.w)) with Inline;
    function wxxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x, vec.x)) with Inline;
    function wxxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x, vec.y)) with Inline;
    function wxxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x, vec.z)) with Inline;
    function wxxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.x, vec.w)) with Inline;
    function wxyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y, vec.x)) with Inline;
    function wxyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y, vec.y)) with Inline;
    function wxyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y, vec.z)) with Inline;
    function wxyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.y, vec.w)) with Inline;
    function wxzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z, vec.x)) with Inline;
    function wxzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z, vec.y)) with Inline;
    function wxzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z, vec.z)) with Inline;
    function wxzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.z, vec.w)) with Inline;
    function wxwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w, vec.x)) with Inline;
    function wxwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w, vec.y)) with Inline;
    function wxwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w, vec.z)) with Inline;
    function wxww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.x, vec.w, vec.w)) with Inline;
    function wyxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x, vec.x)) with Inline;
    function wyxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x, vec.y)) with Inline;
    function wyxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x, vec.z)) with Inline;
    function wyxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.x, vec.w)) with Inline;
    function wyyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y, vec.x)) with Inline;
    function wyyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y, vec.y)) with Inline;
    function wyyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y, vec.z)) with Inline;
    function wyyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.y, vec.w)) with Inline;
    function wyzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z, vec.x)) with Inline;
    function wyzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z, vec.y)) with Inline;
    function wyzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z, vec.z)) with Inline;
    function wyzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.z, vec.w)) with Inline;
    function wywx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w, vec.x)) with Inline;
    function wywy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w, vec.y)) with Inline;
    function wywz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w, vec.z)) with Inline;
    function wyww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.y, vec.w, vec.w)) with Inline;
    function wzxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x, vec.x)) with Inline;
    function wzxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x, vec.y)) with Inline;
    function wzxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x, vec.z)) with Inline;
    function wzxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.x, vec.w)) with Inline;
    function wzyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y, vec.x)) with Inline;
    function wzyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y, vec.y)) with Inline;
    function wzyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y, vec.z)) with Inline;
    function wzyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.y, vec.w)) with Inline;
    function wzzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z, vec.x)) with Inline;
    function wzzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z, vec.y)) with Inline;
    function wzzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z, vec.z)) with Inline;
    function wzzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.z, vec.w)) with Inline;
    function wzwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w, vec.x)) with Inline;
    function wzwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w, vec.y)) with Inline;
    function wzwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w, vec.z)) with Inline;
    function wzww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.z, vec.w, vec.w)) with Inline;
    function wwxx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x, vec.x)) with Inline;
    function wwxy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x, vec.y)) with Inline;
    function wwxz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x, vec.z)) with Inline;
    function wwxw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.x, vec.w)) with Inline;
    function wwyx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y, vec.x)) with Inline;
    function wwyy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y, vec.y)) with Inline;
    function wwyz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y, vec.z)) with Inline;
    function wwyw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.y, vec.w)) with Inline;
    function wwzx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z, vec.x)) with Inline;
    function wwzy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z, vec.y)) with Inline;
    function wwzz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z, vec.z)) with Inline;
    function wwzw (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.z, vec.w)) with Inline;
    function wwwx (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w, vec.x)) with Inline;
    function wwwy (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w, vec.y)) with Inline;
    function wwwz (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w, vec.z)) with Inline;
    function wwww (vec : in     Vkm_GenType) return Vkm_GenType is
        (Make(vec.y, vec.w, vec.w, vec.w)) with Inline;
        
    function rrrr (vec : in     Vkm_GenType) return Vkm_GenType renames xxxx;
    function rrrg (vec : in     Vkm_GenType) return Vkm_GenType renames xxxy;
    function rrrb (vec : in     Vkm_GenType) return Vkm_GenType renames xxxz;
    function rrra (vec : in     Vkm_GenType) return Vkm_GenType renames xxxw;
    function rrgr (vec : in     Vkm_GenType) return Vkm_GenType renames xxyx;
    function rrgg (vec : in     Vkm_GenType) return Vkm_GenType renames xxyy;
    function rrgb (vec : in     Vkm_GenType) return Vkm_GenType renames xxyz;
    function rrga (vec : in     Vkm_GenType) return Vkm_GenType renames xxyw;
    function rrbr (vec : in     Vkm_GenType) return Vkm_GenType renames xxzx;
    function rrbg (vec : in     Vkm_GenType) return Vkm_GenType renames xxzy;
    function rrbb (vec : in     Vkm_GenType) return Vkm_GenType renames xxzz;
    function rrba (vec : in     Vkm_GenType) return Vkm_GenType renames xxzw;
    function rrar (vec : in     Vkm_GenType) return Vkm_GenType renames xxwx;
    function rrag (vec : in     Vkm_GenType) return Vkm_GenType renames xxwy;
    function rrab (vec : in     Vkm_GenType) return Vkm_GenType renames xxwz;
    function rraa (vec : in     Vkm_GenType) return Vkm_GenType renames xxww;
    function rgrr (vec : in     Vkm_GenType) return Vkm_GenType renames xyxx;
    function rgrg (vec : in     Vkm_GenType) return Vkm_GenType renames xyxy;
    function rgrb (vec : in     Vkm_GenType) return Vkm_GenType renames xyxz;
    function rgra (vec : in     Vkm_GenType) return Vkm_GenType renames xyxw;
    function rggr (vec : in     Vkm_GenType) return Vkm_GenType renames xyyx;
    function rggg (vec : in     Vkm_GenType) return Vkm_GenType renames xyyy;
    function rggb (vec : in     Vkm_GenType) return Vkm_GenType renames xyyz;
    function rgga (vec : in     Vkm_GenType) return Vkm_GenType renames xyyw;
    function rgbr (vec : in     Vkm_GenType) return Vkm_GenType renames xyzx;
    function rgbg (vec : in     Vkm_GenType) return Vkm_GenType renames xyzy;
    function rgbb (vec : in     Vkm_GenType) return Vkm_GenType renames xyzz;
    function rgba (vec : in     Vkm_GenType) return Vkm_GenType renames xyzw;
    function rgar (vec : in     Vkm_GenType) return Vkm_GenType renames xywx;
    function rgag (vec : in     Vkm_GenType) return Vkm_GenType renames xywy;
    function rgab (vec : in     Vkm_GenType) return Vkm_GenType renames xywz;
    function rgaa (vec : in     Vkm_GenType) return Vkm_GenType renames xyww;
    function rbrr (vec : in     Vkm_GenType) return Vkm_GenType renames xzxx;
    function rbrg (vec : in     Vkm_GenType) return Vkm_GenType renames xzxy;
    function rbrb (vec : in     Vkm_GenType) return Vkm_GenType renames xzxz;
    function rbra (vec : in     Vkm_GenType) return Vkm_GenType renames xzxw;
    function rbgr (vec : in     Vkm_GenType) return Vkm_GenType renames xzyx;
    function rbgg (vec : in     Vkm_GenType) return Vkm_GenType renames xzyy;
    function rbgb (vec : in     Vkm_GenType) return Vkm_GenType renames xzyz;
    function rbga (vec : in     Vkm_GenType) return Vkm_GenType renames xzyw;
    function rbbr (vec : in     Vkm_GenType) return Vkm_GenType renames xzzx;
    function rbbg (vec : in     Vkm_GenType) return Vkm_GenType renames xzzy;
    function rbbb (vec : in     Vkm_GenType) return Vkm_GenType renames xzzz;
    function rbba (vec : in     Vkm_GenType) return Vkm_GenType renames xzzw;
    function rbar (vec : in     Vkm_GenType) return Vkm_GenType renames xzwx;
    function rbag (vec : in     Vkm_GenType) return Vkm_GenType renames xzwy;
    function rbab (vec : in     Vkm_GenType) return Vkm_GenType renames xzwz;
    function rbaa (vec : in     Vkm_GenType) return Vkm_GenType renames xzww;
    function rarr (vec : in     Vkm_GenType) return Vkm_GenType renames xwxx;
    function rarg (vec : in     Vkm_GenType) return Vkm_GenType renames xwxy;
    function rarb (vec : in     Vkm_GenType) return Vkm_GenType renames xwxz;
    function rara (vec : in     Vkm_GenType) return Vkm_GenType renames xwxw;
    function ragr (vec : in     Vkm_GenType) return Vkm_GenType renames xwyx;
    function ragg (vec : in     Vkm_GenType) return Vkm_GenType renames xwyy;
    function ragb (vec : in     Vkm_GenType) return Vkm_GenType renames xwyz;
    function raga (vec : in     Vkm_GenType) return Vkm_GenType renames xwyw;
    function rabr (vec : in     Vkm_GenType) return Vkm_GenType renames xwzx;
    function rabg (vec : in     Vkm_GenType) return Vkm_GenType renames xwzy;
    function rabb (vec : in     Vkm_GenType) return Vkm_GenType renames xwzz;
    function raba (vec : in     Vkm_GenType) return Vkm_GenType renames xwzw;
    function raar (vec : in     Vkm_GenType) return Vkm_GenType renames xwwx;
    function raag (vec : in     Vkm_GenType) return Vkm_GenType renames xwwy;
    function raab (vec : in     Vkm_GenType) return Vkm_GenType renames xwwz;
    function raaa (vec : in     Vkm_GenType) return Vkm_GenType renames xwww;
    function grrr (vec : in     Vkm_GenType) return Vkm_GenType renames yxxx;
    function grrg (vec : in     Vkm_GenType) return Vkm_GenType renames yxxy;
    function grrb (vec : in     Vkm_GenType) return Vkm_GenType renames yxxz;
    function grra (vec : in     Vkm_GenType) return Vkm_GenType renames yxxw;
    function grgr (vec : in     Vkm_GenType) return Vkm_GenType renames yxyx;
    function grgg (vec : in     Vkm_GenType) return Vkm_GenType renames yxyy;
    function grgb (vec : in     Vkm_GenType) return Vkm_GenType renames yxyz;
    function grga (vec : in     Vkm_GenType) return Vkm_GenType renames yxyw;
    function grbr (vec : in     Vkm_GenType) return Vkm_GenType renames yxzx;
    function grbg (vec : in     Vkm_GenType) return Vkm_GenType renames yxzy;
    function grbb (vec : in     Vkm_GenType) return Vkm_GenType renames yxzz;
    function grba (vec : in     Vkm_GenType) return Vkm_GenType renames yxzw;
    function grar (vec : in     Vkm_GenType) return Vkm_GenType renames yxwx;
    function grag (vec : in     Vkm_GenType) return Vkm_GenType renames yxwy;
    function grab (vec : in     Vkm_GenType) return Vkm_GenType renames yxwz;
    function graa (vec : in     Vkm_GenType) return Vkm_GenType renames yxww;
    function ggrr (vec : in     Vkm_GenType) return Vkm_GenType renames yyxx;
    function ggrg (vec : in     Vkm_GenType) return Vkm_GenType renames yyxy;
    function ggrb (vec : in     Vkm_GenType) return Vkm_GenType renames yyxz;
    function ggra (vec : in     Vkm_GenType) return Vkm_GenType renames yyxw;
    function gggr (vec : in     Vkm_GenType) return Vkm_GenType renames yyyx;
    function gggg (vec : in     Vkm_GenType) return Vkm_GenType renames yyyy;
    function gggb (vec : in     Vkm_GenType) return Vkm_GenType renames yyyz;
    function ggga (vec : in     Vkm_GenType) return Vkm_GenType renames yyyw;
    function ggbr (vec : in     Vkm_GenType) return Vkm_GenType renames yyzx;
    function ggbg (vec : in     Vkm_GenType) return Vkm_GenType renames yyzy;
    function ggbb (vec : in     Vkm_GenType) return Vkm_GenType renames yyzz;
    function ggba (vec : in     Vkm_GenType) return Vkm_GenType renames yyzw;
    function ggar (vec : in     Vkm_GenType) return Vkm_GenType renames yywx;
    function ggag (vec : in     Vkm_GenType) return Vkm_GenType renames yywy;
    function ggab (vec : in     Vkm_GenType) return Vkm_GenType renames yywz;
    function ggaa (vec : in     Vkm_GenType) return Vkm_GenType renames yyww;
    function gbrr (vec : in     Vkm_GenType) return Vkm_GenType renames yzxx;
    function gbrg (vec : in     Vkm_GenType) return Vkm_GenType renames yzxy;
    function gbrb (vec : in     Vkm_GenType) return Vkm_GenType renames yzxz;
    function gbra (vec : in     Vkm_GenType) return Vkm_GenType renames yzxw;
    function gbgr (vec : in     Vkm_GenType) return Vkm_GenType renames yzyx;
    function gbgg (vec : in     Vkm_GenType) return Vkm_GenType renames yzyy;
    function gbgb (vec : in     Vkm_GenType) return Vkm_GenType renames yzyz;
    function gbga (vec : in     Vkm_GenType) return Vkm_GenType renames yzyw;
    function gbbr (vec : in     Vkm_GenType) return Vkm_GenType renames yzzx;
    function gbbg (vec : in     Vkm_GenType) return Vkm_GenType renames yzzy;
    function gbbb (vec : in     Vkm_GenType) return Vkm_GenType renames yzzz;
    function gbba (vec : in     Vkm_GenType) return Vkm_GenType renames yzzw;
    function gbar (vec : in     Vkm_GenType) return Vkm_GenType renames yzwx;
    function gbag (vec : in     Vkm_GenType) return Vkm_GenType renames yzwy;
    function gbab (vec : in     Vkm_GenType) return Vkm_GenType renames yzwz;
    function gbaa (vec : in     Vkm_GenType) return Vkm_GenType renames yzww;
    function garr (vec : in     Vkm_GenType) return Vkm_GenType renames ywxx;
    function garg (vec : in     Vkm_GenType) return Vkm_GenType renames ywxy;
    function garb (vec : in     Vkm_GenType) return Vkm_GenType renames ywxz;
    function gara (vec : in     Vkm_GenType) return Vkm_GenType renames ywxw;
    function gagr (vec : in     Vkm_GenType) return Vkm_GenType renames ywyx;
    function gagg (vec : in     Vkm_GenType) return Vkm_GenType renames ywyy;
    function gagb (vec : in     Vkm_GenType) return Vkm_GenType renames ywyz;
    function gaga (vec : in     Vkm_GenType) return Vkm_GenType renames ywyw;
    function gabr (vec : in     Vkm_GenType) return Vkm_GenType renames ywzx;
    function gabg (vec : in     Vkm_GenType) return Vkm_GenType renames ywzy;
    function gabb (vec : in     Vkm_GenType) return Vkm_GenType renames ywzz;
    function gaba (vec : in     Vkm_GenType) return Vkm_GenType renames ywzw;
    function gaar (vec : in     Vkm_GenType) return Vkm_GenType renames ywwx;
    function gaag (vec : in     Vkm_GenType) return Vkm_GenType renames ywwy;
    function gaab (vec : in     Vkm_GenType) return Vkm_GenType renames ywwz;
    function gaaa (vec : in     Vkm_GenType) return Vkm_GenType renames ywww;
    function brrr (vec : in     Vkm_GenType) return Vkm_GenType renames zxxx;
    function brrg (vec : in     Vkm_GenType) return Vkm_GenType renames zxxy;
    function brrb (vec : in     Vkm_GenType) return Vkm_GenType renames zxxz;
    function brra (vec : in     Vkm_GenType) return Vkm_GenType renames zxxw;
    function brgr (vec : in     Vkm_GenType) return Vkm_GenType renames zxyx;
    function brgg (vec : in     Vkm_GenType) return Vkm_GenType renames zxyy;
    function brgb (vec : in     Vkm_GenType) return Vkm_GenType renames zxyz;
    function brga (vec : in     Vkm_GenType) return Vkm_GenType renames zxyw;
    function brbr (vec : in     Vkm_GenType) return Vkm_GenType renames zxzx;
    function brbg (vec : in     Vkm_GenType) return Vkm_GenType renames zxzy;
    function brbb (vec : in     Vkm_GenType) return Vkm_GenType renames zxzz;
    function brba (vec : in     Vkm_GenType) return Vkm_GenType renames zxzw;
    function brar (vec : in     Vkm_GenType) return Vkm_GenType renames zxwx;
    function brag (vec : in     Vkm_GenType) return Vkm_GenType renames zxwy;
    function brab (vec : in     Vkm_GenType) return Vkm_GenType renames zxwz;
    function braa (vec : in     Vkm_GenType) return Vkm_GenType renames zxww;
    function bgrr (vec : in     Vkm_GenType) return Vkm_GenType renames zyxx;
    function bgrg (vec : in     Vkm_GenType) return Vkm_GenType renames zyxy;
    function bgrb (vec : in     Vkm_GenType) return Vkm_GenType renames zyxz;
    function bgra (vec : in     Vkm_GenType) return Vkm_GenType renames zyxw;
    function bggr (vec : in     Vkm_GenType) return Vkm_GenType renames zyyx;
    function bggg (vec : in     Vkm_GenType) return Vkm_GenType renames zyyy;
    function bggb (vec : in     Vkm_GenType) return Vkm_GenType renames zyyz;
    function bgga (vec : in     Vkm_GenType) return Vkm_GenType renames zyyw;
    function bgbr (vec : in     Vkm_GenType) return Vkm_GenType renames zyzx;
    function bgbg (vec : in     Vkm_GenType) return Vkm_GenType renames zyzy;
    function bgbb (vec : in     Vkm_GenType) return Vkm_GenType renames zyzz;
    function bgba (vec : in     Vkm_GenType) return Vkm_GenType renames zyzw;
    function bgar (vec : in     Vkm_GenType) return Vkm_GenType renames zywx;
    function bgag (vec : in     Vkm_GenType) return Vkm_GenType renames zywy;
    function bgab (vec : in     Vkm_GenType) return Vkm_GenType renames zywz;
    function bgaa (vec : in     Vkm_GenType) return Vkm_GenType renames zyww;
    function bbrr (vec : in     Vkm_GenType) return Vkm_GenType renames zzxx;
    function bbrg (vec : in     Vkm_GenType) return Vkm_GenType renames zzxy;
    function bbrb (vec : in     Vkm_GenType) return Vkm_GenType renames zzxz;
    function bbra (vec : in     Vkm_GenType) return Vkm_GenType renames zzxw;
    function bbgr (vec : in     Vkm_GenType) return Vkm_GenType renames zzyx;
    function bbgg (vec : in     Vkm_GenType) return Vkm_GenType renames zzyy;
    function bbgb (vec : in     Vkm_GenType) return Vkm_GenType renames zzyz;
    function bbga (vec : in     Vkm_GenType) return Vkm_GenType renames zzyw;
    function bbbr (vec : in     Vkm_GenType) return Vkm_GenType renames zzzx;
    function bbbg (vec : in     Vkm_GenType) return Vkm_GenType renames zzzy;
    function bbbb (vec : in     Vkm_GenType) return Vkm_GenType renames zzzz;
    function bbba (vec : in     Vkm_GenType) return Vkm_GenType renames zzzw;
    function bbar (vec : in     Vkm_GenType) return Vkm_GenType renames zzwx;
    function bbag (vec : in     Vkm_GenType) return Vkm_GenType renames zzwy;
    function bbab (vec : in     Vkm_GenType) return Vkm_GenType renames zzwz;
    function bbaa (vec : in     Vkm_GenType) return Vkm_GenType renames zzww;
    function barr (vec : in     Vkm_GenType) return Vkm_GenType renames zwxx;
    function barg (vec : in     Vkm_GenType) return Vkm_GenType renames zwxy;
    function barb (vec : in     Vkm_GenType) return Vkm_GenType renames zwxz;
    function bara (vec : in     Vkm_GenType) return Vkm_GenType renames zwxw;
    function bagr (vec : in     Vkm_GenType) return Vkm_GenType renames zwyx;
    function bagg (vec : in     Vkm_GenType) return Vkm_GenType renames zwyy;
    function bagb (vec : in     Vkm_GenType) return Vkm_GenType renames zwyz;
    function baga (vec : in     Vkm_GenType) return Vkm_GenType renames zwyw;
    function babr (vec : in     Vkm_GenType) return Vkm_GenType renames zwzx;
    function babg (vec : in     Vkm_GenType) return Vkm_GenType renames zwzy;
    function babb (vec : in     Vkm_GenType) return Vkm_GenType renames zwzz;
    function baba (vec : in     Vkm_GenType) return Vkm_GenType renames zwzw;
    function baar (vec : in     Vkm_GenType) return Vkm_GenType renames zwwx;
    function baag (vec : in     Vkm_GenType) return Vkm_GenType renames zwwy;
    function baab (vec : in     Vkm_GenType) return Vkm_GenType renames zwwz;
    function baaa (vec : in     Vkm_GenType) return Vkm_GenType renames zwww;
    function arrr (vec : in     Vkm_GenType) return Vkm_GenType renames wxxx;
    function arrg (vec : in     Vkm_GenType) return Vkm_GenType renames wxxy;
    function arrb (vec : in     Vkm_GenType) return Vkm_GenType renames wxxz;
    function arra (vec : in     Vkm_GenType) return Vkm_GenType renames wxxw;
    function argr (vec : in     Vkm_GenType) return Vkm_GenType renames wxyx;
    function argg (vec : in     Vkm_GenType) return Vkm_GenType renames wxyy;
    function argb (vec : in     Vkm_GenType) return Vkm_GenType renames wxyz;
    function arga (vec : in     Vkm_GenType) return Vkm_GenType renames wxyw;
    function arbr (vec : in     Vkm_GenType) return Vkm_GenType renames wxzx;
    function arbg (vec : in     Vkm_GenType) return Vkm_GenType renames wxzy;
    function arbb (vec : in     Vkm_GenType) return Vkm_GenType renames wxzz;
    function arba (vec : in     Vkm_GenType) return Vkm_GenType renames wxzw;
    function arar (vec : in     Vkm_GenType) return Vkm_GenType renames wxwx;
    function arag (vec : in     Vkm_GenType) return Vkm_GenType renames wxwy;
    function arab (vec : in     Vkm_GenType) return Vkm_GenType renames wxwz;
    function araa (vec : in     Vkm_GenType) return Vkm_GenType renames wxww;
    function agrr (vec : in     Vkm_GenType) return Vkm_GenType renames wyxx;
    function agrg (vec : in     Vkm_GenType) return Vkm_GenType renames wyxy;
    function agrb (vec : in     Vkm_GenType) return Vkm_GenType renames wyxz;
    function agra (vec : in     Vkm_GenType) return Vkm_GenType renames wyxw;
    function aggr (vec : in     Vkm_GenType) return Vkm_GenType renames wyyx;
    function aggg (vec : in     Vkm_GenType) return Vkm_GenType renames wyyy;
    function aggb (vec : in     Vkm_GenType) return Vkm_GenType renames wyyz;
    function agga (vec : in     Vkm_GenType) return Vkm_GenType renames wyyw;
    function agbr (vec : in     Vkm_GenType) return Vkm_GenType renames wyzx;
    function agbg (vec : in     Vkm_GenType) return Vkm_GenType renames wyzy;
    function agbb (vec : in     Vkm_GenType) return Vkm_GenType renames wyzz;
    function agba (vec : in     Vkm_GenType) return Vkm_GenType renames wyzw;
    function agar (vec : in     Vkm_GenType) return Vkm_GenType renames wywx;
    function agag (vec : in     Vkm_GenType) return Vkm_GenType renames wywy;
    function agab (vec : in     Vkm_GenType) return Vkm_GenType renames wywz;
    function agaa (vec : in     Vkm_GenType) return Vkm_GenType renames wyww;
    function abrr (vec : in     Vkm_GenType) return Vkm_GenType renames wzxx;
    function abrg (vec : in     Vkm_GenType) return Vkm_GenType renames wzxy;
    function abrb (vec : in     Vkm_GenType) return Vkm_GenType renames wzxz;
    function abra (vec : in     Vkm_GenType) return Vkm_GenType renames wzxw;
    function abgr (vec : in     Vkm_GenType) return Vkm_GenType renames wzyx;
    function abgg (vec : in     Vkm_GenType) return Vkm_GenType renames wzyy;
    function abgb (vec : in     Vkm_GenType) return Vkm_GenType renames wzyz;
    function abga (vec : in     Vkm_GenType) return Vkm_GenType renames wzyw;
    function abbr (vec : in     Vkm_GenType) return Vkm_GenType renames wzzx;
    function abbg (vec : in     Vkm_GenType) return Vkm_GenType renames wzzy;
    function abbb (vec : in     Vkm_GenType) return Vkm_GenType renames wzzz;
    function abba (vec : in     Vkm_GenType) return Vkm_GenType renames wzzw;
    function abar (vec : in     Vkm_GenType) return Vkm_GenType renames wzwx;
    function abag (vec : in     Vkm_GenType) return Vkm_GenType renames wzwy;
    function abab (vec : in     Vkm_GenType) return Vkm_GenType renames wzwz;
    function abaa (vec : in     Vkm_GenType) return Vkm_GenType renames wzww;
    function aarr (vec : in     Vkm_GenType) return Vkm_GenType renames wwxx;
    function aarg (vec : in     Vkm_GenType) return Vkm_GenType renames wwxy;
    function aarb (vec : in     Vkm_GenType) return Vkm_GenType renames wwxz;
    function aara (vec : in     Vkm_GenType) return Vkm_GenType renames wwxw;
    function aagr (vec : in     Vkm_GenType) return Vkm_GenType renames wwyx;
    function aagg (vec : in     Vkm_GenType) return Vkm_GenType renames wwyy;
    function aagb (vec : in     Vkm_GenType) return Vkm_GenType renames wwyz;
    function aaga (vec : in     Vkm_GenType) return Vkm_GenType renames wwyw;
    function aabr (vec : in     Vkm_GenType) return Vkm_GenType renames wwzx;
    function aabg (vec : in     Vkm_GenType) return Vkm_GenType renames wwzy;
    function aabb (vec : in     Vkm_GenType) return Vkm_GenType renames wwzz;
    function aaba (vec : in     Vkm_GenType) return Vkm_GenType renames wwzw;
    function aaar (vec : in     Vkm_GenType) return Vkm_GenType renames wwwx;
    function aaag (vec : in     Vkm_GenType) return Vkm_GenType renames wwwy;
    function aaab (vec : in     Vkm_GenType) return Vkm_GenType renames wwwz;
    function aaaa (vec : in     Vkm_GenType) return Vkm_GenType renames wwww;
        
    function ssss (vec : in     Vkm_GenType) return Vkm_GenType renames xxxx;
    function ssst (vec : in     Vkm_GenType) return Vkm_GenType renames xxxy;
    function sssp (vec : in     Vkm_GenType) return Vkm_GenType renames xxxz;
    function sssq (vec : in     Vkm_GenType) return Vkm_GenType renames xxxw;
    function ssts (vec : in     Vkm_GenType) return Vkm_GenType renames xxyx;
    function sstt (vec : in     Vkm_GenType) return Vkm_GenType renames xxyy;
    function sstp (vec : in     Vkm_GenType) return Vkm_GenType renames xxyz;
    function sstq (vec : in     Vkm_GenType) return Vkm_GenType renames xxyw;
    function ssps (vec : in     Vkm_GenType) return Vkm_GenType renames xxzx;
    function sspt (vec : in     Vkm_GenType) return Vkm_GenType renames xxzy;
    function sspp (vec : in     Vkm_GenType) return Vkm_GenType renames xxzz;
    function sspq (vec : in     Vkm_GenType) return Vkm_GenType renames xxzw;
    function ssqs (vec : in     Vkm_GenType) return Vkm_GenType renames xxwx;
    function ssqt (vec : in     Vkm_GenType) return Vkm_GenType renames xxwy;
    function ssqp (vec : in     Vkm_GenType) return Vkm_GenType renames xxwz;
    function ssqq (vec : in     Vkm_GenType) return Vkm_GenType renames xxww;
    function stss (vec : in     Vkm_GenType) return Vkm_GenType renames xyxx;
    function stst (vec : in     Vkm_GenType) return Vkm_GenType renames xyxy;
    function stsp (vec : in     Vkm_GenType) return Vkm_GenType renames xyxz;
    function stsq (vec : in     Vkm_GenType) return Vkm_GenType renames xyxw;
    function stts (vec : in     Vkm_GenType) return Vkm_GenType renames xyyx;
    function sttt (vec : in     Vkm_GenType) return Vkm_GenType renames xyyy;
    function sttp (vec : in     Vkm_GenType) return Vkm_GenType renames xyyz;
    function sttq (vec : in     Vkm_GenType) return Vkm_GenType renames xyyw;
    function stps (vec : in     Vkm_GenType) return Vkm_GenType renames xyzx;
    function stpt (vec : in     Vkm_GenType) return Vkm_GenType renames xyzy;
    function stpp (vec : in     Vkm_GenType) return Vkm_GenType renames xyzz;
    function stpq (vec : in     Vkm_GenType) return Vkm_GenType renames xyzw;
    function stqs (vec : in     Vkm_GenType) return Vkm_GenType renames xywx;
    function stqt (vec : in     Vkm_GenType) return Vkm_GenType renames xywy;
    function stqp (vec : in     Vkm_GenType) return Vkm_GenType renames xywz;
    function stqq (vec : in     Vkm_GenType) return Vkm_GenType renames xyww;
    function spss (vec : in     Vkm_GenType) return Vkm_GenType renames xzxx;
    function spst (vec : in     Vkm_GenType) return Vkm_GenType renames xzxy;
    function spsp (vec : in     Vkm_GenType) return Vkm_GenType renames xzxz;
    function spsq (vec : in     Vkm_GenType) return Vkm_GenType renames xzxw;
    function spts (vec : in     Vkm_GenType) return Vkm_GenType renames xzyx;
    function sptt (vec : in     Vkm_GenType) return Vkm_GenType renames xzyy;
    function sptp (vec : in     Vkm_GenType) return Vkm_GenType renames xzyz;
    function sptq (vec : in     Vkm_GenType) return Vkm_GenType renames xzyw;
    function spps (vec : in     Vkm_GenType) return Vkm_GenType renames xzzx;
    function sppt (vec : in     Vkm_GenType) return Vkm_GenType renames xzzy;
    function sppp (vec : in     Vkm_GenType) return Vkm_GenType renames xzzz;
    function sppq (vec : in     Vkm_GenType) return Vkm_GenType renames xzzw;
    function spqs (vec : in     Vkm_GenType) return Vkm_GenType renames xzwx;
    function spqt (vec : in     Vkm_GenType) return Vkm_GenType renames xzwy;
    function spqp (vec : in     Vkm_GenType) return Vkm_GenType renames xzwz;
    function spqq (vec : in     Vkm_GenType) return Vkm_GenType renames xzww;
    function sqss (vec : in     Vkm_GenType) return Vkm_GenType renames xwxx;
    function sqst (vec : in     Vkm_GenType) return Vkm_GenType renames xwxy;
    function sqsp (vec : in     Vkm_GenType) return Vkm_GenType renames xwxz;
    function sqsq (vec : in     Vkm_GenType) return Vkm_GenType renames xwxw;
    function sqts (vec : in     Vkm_GenType) return Vkm_GenType renames xwyx;
    function sqtt (vec : in     Vkm_GenType) return Vkm_GenType renames xwyy;
    function sqtp (vec : in     Vkm_GenType) return Vkm_GenType renames xwyz;
    function sqtq (vec : in     Vkm_GenType) return Vkm_GenType renames xwyw;
    function sqps (vec : in     Vkm_GenType) return Vkm_GenType renames xwzx;
    function sqpt (vec : in     Vkm_GenType) return Vkm_GenType renames xwzy;
    function sqpp (vec : in     Vkm_GenType) return Vkm_GenType renames xwzz;
    function sqpq (vec : in     Vkm_GenType) return Vkm_GenType renames xwzw;
    function sqqs (vec : in     Vkm_GenType) return Vkm_GenType renames xwwx;
    function sqqt (vec : in     Vkm_GenType) return Vkm_GenType renames xwwy;
    function sqqp (vec : in     Vkm_GenType) return Vkm_GenType renames xwwz;
    function sqqq (vec : in     Vkm_GenType) return Vkm_GenType renames xwww;        
    function tsss (vec : in     Vkm_GenType) return Vkm_GenType renames yxxx;
    function tsst (vec : in     Vkm_GenType) return Vkm_GenType renames yxxy;
    function tssp (vec : in     Vkm_GenType) return Vkm_GenType renames yxxz;
    function tssq (vec : in     Vkm_GenType) return Vkm_GenType renames yxxw;
    function tsts (vec : in     Vkm_GenType) return Vkm_GenType renames yxyx;
    function tstt (vec : in     Vkm_GenType) return Vkm_GenType renames yxyy;
    function tstp (vec : in     Vkm_GenType) return Vkm_GenType renames yxyz;
    function tstq (vec : in     Vkm_GenType) return Vkm_GenType renames yxyw;
    function tsps (vec : in     Vkm_GenType) return Vkm_GenType renames yxzx;
    function tspt (vec : in     Vkm_GenType) return Vkm_GenType renames yxzy;
    function tspp (vec : in     Vkm_GenType) return Vkm_GenType renames yxzz;
    function tspq (vec : in     Vkm_GenType) return Vkm_GenType renames yxzw;
    function tsqs (vec : in     Vkm_GenType) return Vkm_GenType renames yxwx;
    function tsqt (vec : in     Vkm_GenType) return Vkm_GenType renames yxwy;
    function tsqp (vec : in     Vkm_GenType) return Vkm_GenType renames yxwz;
    function tsqq (vec : in     Vkm_GenType) return Vkm_GenType renames yxww;
    function ttss (vec : in     Vkm_GenType) return Vkm_GenType renames yyxx;
    function ttst (vec : in     Vkm_GenType) return Vkm_GenType renames yyxy;
    function ttsp (vec : in     Vkm_GenType) return Vkm_GenType renames yyxz;
    function ttsq (vec : in     Vkm_GenType) return Vkm_GenType renames yyxw;
    function ttts (vec : in     Vkm_GenType) return Vkm_GenType renames yyyx;
    function tttt (vec : in     Vkm_GenType) return Vkm_GenType renames yyyy;
    function tttp (vec : in     Vkm_GenType) return Vkm_GenType renames yyyz;
    function tttq (vec : in     Vkm_GenType) return Vkm_GenType renames yyyw;
    function ttps (vec : in     Vkm_GenType) return Vkm_GenType renames yyzx;
    function ttpt (vec : in     Vkm_GenType) return Vkm_GenType renames yyzy;
    function ttpp (vec : in     Vkm_GenType) return Vkm_GenType renames yyzz;
    function ttpq (vec : in     Vkm_GenType) return Vkm_GenType renames yyzw;
    function ttqs (vec : in     Vkm_GenType) return Vkm_GenType renames yywx;
    function ttqt (vec : in     Vkm_GenType) return Vkm_GenType renames yywy;
    function ttqp (vec : in     Vkm_GenType) return Vkm_GenType renames yywz;
    function ttqq (vec : in     Vkm_GenType) return Vkm_GenType renames yyww;
    function tpss (vec : in     Vkm_GenType) return Vkm_GenType renames yzxx;
    function tpst (vec : in     Vkm_GenType) return Vkm_GenType renames yzxy;
    function tpsp (vec : in     Vkm_GenType) return Vkm_GenType renames yzxz;
    function tpsq (vec : in     Vkm_GenType) return Vkm_GenType renames yzxw;
    function tpts (vec : in     Vkm_GenType) return Vkm_GenType renames yzyx;
    function tptt (vec : in     Vkm_GenType) return Vkm_GenType renames yzyy;
    function tptp (vec : in     Vkm_GenType) return Vkm_GenType renames yzyz;
    function tptq (vec : in     Vkm_GenType) return Vkm_GenType renames yzyw;
    function tpps (vec : in     Vkm_GenType) return Vkm_GenType renames yzzx;
    function tppt (vec : in     Vkm_GenType) return Vkm_GenType renames yzzy;
    function tppp (vec : in     Vkm_GenType) return Vkm_GenType renames yzzz;
    function tppq (vec : in     Vkm_GenType) return Vkm_GenType renames yzzw;
    function tpqs (vec : in     Vkm_GenType) return Vkm_GenType renames yzwx;
    function tpqt (vec : in     Vkm_GenType) return Vkm_GenType renames yzwy;
    function tpqp (vec : in     Vkm_GenType) return Vkm_GenType renames yzwz;
    function tpqq (vec : in     Vkm_GenType) return Vkm_GenType renames yzww;
    function tqss (vec : in     Vkm_GenType) return Vkm_GenType renames ywxx;
    function tqst (vec : in     Vkm_GenType) return Vkm_GenType renames ywxy;
    function tqsp (vec : in     Vkm_GenType) return Vkm_GenType renames ywxz;
    function tqsq (vec : in     Vkm_GenType) return Vkm_GenType renames ywxw;
    function tqts (vec : in     Vkm_GenType) return Vkm_GenType renames ywyx;
    function tqtt (vec : in     Vkm_GenType) return Vkm_GenType renames ywyy;
    function tqtp (vec : in     Vkm_GenType) return Vkm_GenType renames ywyz;
    function tqtq (vec : in     Vkm_GenType) return Vkm_GenType renames ywyw;
    function tqps (vec : in     Vkm_GenType) return Vkm_GenType renames ywzx;
    function tqpt (vec : in     Vkm_GenType) return Vkm_GenType renames ywzy;
    function tqpp (vec : in     Vkm_GenType) return Vkm_GenType renames ywzz;
    function tqpq (vec : in     Vkm_GenType) return Vkm_GenType renames ywzw;
    function tqqs (vec : in     Vkm_GenType) return Vkm_GenType renames ywwx;
    function tqqt (vec : in     Vkm_GenType) return Vkm_GenType renames ywwy;
    function tqqp (vec : in     Vkm_GenType) return Vkm_GenType renames ywwz;
    function tqqq (vec : in     Vkm_GenType) return Vkm_GenType renames ywww;        
    function psss (vec : in     Vkm_GenType) return Vkm_GenType renames zxxx;
    function psst (vec : in     Vkm_GenType) return Vkm_GenType renames zxxy;
    function pssp (vec : in     Vkm_GenType) return Vkm_GenType renames zxxz;
    function pssq (vec : in     Vkm_GenType) return Vkm_GenType renames zxxw;
    function psts (vec : in     Vkm_GenType) return Vkm_GenType renames zxyx;
    function pstt (vec : in     Vkm_GenType) return Vkm_GenType renames zxyy;
    function pstp (vec : in     Vkm_GenType) return Vkm_GenType renames zxyz;
    function pstq (vec : in     Vkm_GenType) return Vkm_GenType renames zxyw;
    function psps (vec : in     Vkm_GenType) return Vkm_GenType renames zxzx;
    function pspt (vec : in     Vkm_GenType) return Vkm_GenType renames zxzy;
    function pspp (vec : in     Vkm_GenType) return Vkm_GenType renames zxzz;
    function pspq (vec : in     Vkm_GenType) return Vkm_GenType renames zxzw;
    function psqs (vec : in     Vkm_GenType) return Vkm_GenType renames zxwx;
    function psqt (vec : in     Vkm_GenType) return Vkm_GenType renames zxwy;
    function psqp (vec : in     Vkm_GenType) return Vkm_GenType renames zxwz;
    function psqq (vec : in     Vkm_GenType) return Vkm_GenType renames zxww;
    function ptss (vec : in     Vkm_GenType) return Vkm_GenType renames zyxx;
    function ptst (vec : in     Vkm_GenType) return Vkm_GenType renames zyxy;
    function ptsp (vec : in     Vkm_GenType) return Vkm_GenType renames zyxz;
    function ptsq (vec : in     Vkm_GenType) return Vkm_GenType renames zyxw;
    function ptts (vec : in     Vkm_GenType) return Vkm_GenType renames zyyx;
    function pttt (vec : in     Vkm_GenType) return Vkm_GenType renames zyyy;
    function pttp (vec : in     Vkm_GenType) return Vkm_GenType renames zyyz;
    function pttq (vec : in     Vkm_GenType) return Vkm_GenType renames zyyw;
    function ptps (vec : in     Vkm_GenType) return Vkm_GenType renames zyzx;
    function ptpt (vec : in     Vkm_GenType) return Vkm_GenType renames zyzy;
    function ptpp (vec : in     Vkm_GenType) return Vkm_GenType renames zyzz;
    function ptpq (vec : in     Vkm_GenType) return Vkm_GenType renames zyzw;
    function ptqs (vec : in     Vkm_GenType) return Vkm_GenType renames zywx;
    function ptqt (vec : in     Vkm_GenType) return Vkm_GenType renames zywy;
    function ptqp (vec : in     Vkm_GenType) return Vkm_GenType renames zywz;
    function ptqq (vec : in     Vkm_GenType) return Vkm_GenType renames zyww;
    function ppss (vec : in     Vkm_GenType) return Vkm_GenType renames zzxx;
    function ppst (vec : in     Vkm_GenType) return Vkm_GenType renames zzxy;
    function ppsp (vec : in     Vkm_GenType) return Vkm_GenType renames zzxz;
    function ppsq (vec : in     Vkm_GenType) return Vkm_GenType renames zzxw;
    function ppts (vec : in     Vkm_GenType) return Vkm_GenType renames zzyx;
    function pptt (vec : in     Vkm_GenType) return Vkm_GenType renames zzyy;
    function pptp (vec : in     Vkm_GenType) return Vkm_GenType renames zzyz;
    function pptq (vec : in     Vkm_GenType) return Vkm_GenType renames zzyw;
    function ppps (vec : in     Vkm_GenType) return Vkm_GenType renames zzzx;
    function pppt (vec : in     Vkm_GenType) return Vkm_GenType renames zzzy;
    function pppp (vec : in     Vkm_GenType) return Vkm_GenType renames zzzz;
    function pppq (vec : in     Vkm_GenType) return Vkm_GenType renames zzzw;
    function ppqs (vec : in     Vkm_GenType) return Vkm_GenType renames zzwx;
    function ppqt (vec : in     Vkm_GenType) return Vkm_GenType renames zzwy;
    function ppqp (vec : in     Vkm_GenType) return Vkm_GenType renames zzwz;
    function ppqq (vec : in     Vkm_GenType) return Vkm_GenType renames zzww;
    function pqss (vec : in     Vkm_GenType) return Vkm_GenType renames zwxx;
    function pqst (vec : in     Vkm_GenType) return Vkm_GenType renames zwxy;
    function pqsp (vec : in     Vkm_GenType) return Vkm_GenType renames zwxz;
    function pqsq (vec : in     Vkm_GenType) return Vkm_GenType renames zwxw;
    function pqts (vec : in     Vkm_GenType) return Vkm_GenType renames zwyx;
    function pqtt (vec : in     Vkm_GenType) return Vkm_GenType renames zwyy;
    function pqtp (vec : in     Vkm_GenType) return Vkm_GenType renames zwyz;
    function pqtq (vec : in     Vkm_GenType) return Vkm_GenType renames zwyw;
    function pqps (vec : in     Vkm_GenType) return Vkm_GenType renames zwzx;
    function pqpt (vec : in     Vkm_GenType) return Vkm_GenType renames zwzy;
    function pqpp (vec : in     Vkm_GenType) return Vkm_GenType renames zwzz;
    function pqpq (vec : in     Vkm_GenType) return Vkm_GenType renames zwzw;
    function pqqs (vec : in     Vkm_GenType) return Vkm_GenType renames zwwx;
    function pqqt (vec : in     Vkm_GenType) return Vkm_GenType renames zwwy;
    function pqqp (vec : in     Vkm_GenType) return Vkm_GenType renames zwwz;
    function pqqq (vec : in     Vkm_GenType) return Vkm_GenType renames zwww;        
    function qsss (vec : in     Vkm_GenType) return Vkm_GenType renames wxxx;
    function qsst (vec : in     Vkm_GenType) return Vkm_GenType renames wxxy;
    function qssp (vec : in     Vkm_GenType) return Vkm_GenType renames wxxz;
    function qssq (vec : in     Vkm_GenType) return Vkm_GenType renames wxxw;
    function qsts (vec : in     Vkm_GenType) return Vkm_GenType renames wxyx;
    function qstt (vec : in     Vkm_GenType) return Vkm_GenType renames wxyy;
    function qstp (vec : in     Vkm_GenType) return Vkm_GenType renames wxyz;
    function qstq (vec : in     Vkm_GenType) return Vkm_GenType renames wxyw;
    function qsps (vec : in     Vkm_GenType) return Vkm_GenType renames wxzx;
    function qspt (vec : in     Vkm_GenType) return Vkm_GenType renames wxzy;
    function qspp (vec : in     Vkm_GenType) return Vkm_GenType renames wxzz;
    function qspq (vec : in     Vkm_GenType) return Vkm_GenType renames wxzw;
    function qsqs (vec : in     Vkm_GenType) return Vkm_GenType renames wxwx;
    function qsqt (vec : in     Vkm_GenType) return Vkm_GenType renames wxwy;
    function qsqp (vec : in     Vkm_GenType) return Vkm_GenType renames wxwz;
    function qsqq (vec : in     Vkm_GenType) return Vkm_GenType renames wxww;
    function qtss (vec : in     Vkm_GenType) return Vkm_GenType renames wyxx;
    function qtst (vec : in     Vkm_GenType) return Vkm_GenType renames wyxy;
    function qtsp (vec : in     Vkm_GenType) return Vkm_GenType renames wyxz;
    function qtsq (vec : in     Vkm_GenType) return Vkm_GenType renames wyxw;
    function qtts (vec : in     Vkm_GenType) return Vkm_GenType renames wyyx;
    function qttt (vec : in     Vkm_GenType) return Vkm_GenType renames wyyy;
    function qttp (vec : in     Vkm_GenType) return Vkm_GenType renames wyyz;
    function qttq (vec : in     Vkm_GenType) return Vkm_GenType renames wyyw;
    function qtps (vec : in     Vkm_GenType) return Vkm_GenType renames wyzx;
    function qtpt (vec : in     Vkm_GenType) return Vkm_GenType renames wyzy;
    function qtpp (vec : in     Vkm_GenType) return Vkm_GenType renames wyzz;
    function qtpq (vec : in     Vkm_GenType) return Vkm_GenType renames wyzw;
    function qtqs (vec : in     Vkm_GenType) return Vkm_GenType renames wywx;
    function qtqt (vec : in     Vkm_GenType) return Vkm_GenType renames wywy;
    function qtqp (vec : in     Vkm_GenType) return Vkm_GenType renames wywz;
    function qtqq (vec : in     Vkm_GenType) return Vkm_GenType renames wyww;
    function qpss (vec : in     Vkm_GenType) return Vkm_GenType renames wzxx;
    function qpst (vec : in     Vkm_GenType) return Vkm_GenType renames wzxy;
    function qpsp (vec : in     Vkm_GenType) return Vkm_GenType renames wzxz;
    function qpsq (vec : in     Vkm_GenType) return Vkm_GenType renames wzxw;
    function qpts (vec : in     Vkm_GenType) return Vkm_GenType renames wzyx;
    function qptt (vec : in     Vkm_GenType) return Vkm_GenType renames wzyy;
    function qptp (vec : in     Vkm_GenType) return Vkm_GenType renames wzyz;
    function qptq (vec : in     Vkm_GenType) return Vkm_GenType renames wzyw;
    function qpps (vec : in     Vkm_GenType) return Vkm_GenType renames wzzx;
    function qppt (vec : in     Vkm_GenType) return Vkm_GenType renames wzzy;
    function qppp (vec : in     Vkm_GenType) return Vkm_GenType renames wzzz;
    function qppq (vec : in     Vkm_GenType) return Vkm_GenType renames wzzw;
    function qpqs (vec : in     Vkm_GenType) return Vkm_GenType renames wzwx;
    function qpqt (vec : in     Vkm_GenType) return Vkm_GenType renames wzwy;
    function qpqp (vec : in     Vkm_GenType) return Vkm_GenType renames wzwz;
    function qpqq (vec : in     Vkm_GenType) return Vkm_GenType renames wzww;
    function qqss (vec : in     Vkm_GenType) return Vkm_GenType renames wwxx;
    function qqst (vec : in     Vkm_GenType) return Vkm_GenType renames wwxy;
    function qqsp (vec : in     Vkm_GenType) return Vkm_GenType renames wwxz;
    function qqsq (vec : in     Vkm_GenType) return Vkm_GenType renames wwxw;
    function qqts (vec : in     Vkm_GenType) return Vkm_GenType renames wwyx;
    function qqtt (vec : in     Vkm_GenType) return Vkm_GenType renames wwyy;
    function qqtp (vec : in     Vkm_GenType) return Vkm_GenType renames wwyz;
    function qqtq (vec : in     Vkm_GenType) return Vkm_GenType renames wwyw;
    function qqps (vec : in     Vkm_GenType) return Vkm_GenType renames wwzx;
    function qqpt (vec : in     Vkm_GenType) return Vkm_GenType renames wwzy;
    function qqpp (vec : in     Vkm_GenType) return Vkm_GenType renames wwzz;
    function qqpq (vec : in     Vkm_GenType) return Vkm_GenType renames wwzw;
    function qqqs (vec : in     Vkm_GenType) return Vkm_GenType renames wwwx;
    function qqqt (vec : in     Vkm_GenType) return Vkm_GenType renames wwwy;
    function qqqp (vec : in     Vkm_GenType) return Vkm_GenType renames wwwz;
    function qqqq (vec : in     Vkm_GenType) return Vkm_GenType renames wwww;

    -- Set
    procedure xyzw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure xywz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure xzyw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure xzwy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure xwyz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure xwzy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure yxzw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure yxwz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure yzxw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure yzwx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure ywxz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure ywzx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure zxyw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure zxwy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure zyxw (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure zywx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure zwxy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure zwyx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure wxyz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure wxzy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure wyxz (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure wyzx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure wzxy (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);
    procedure wzyx (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType);

    procedure rgba (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xyzw;
    procedure rgab (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xywz;
    procedure rbga (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzyw;
    procedure rbag (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xzwy;
    procedure ragb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwyz;
    procedure rabg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames xwzy;
    procedure grba (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxzw;
    procedure grab (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yxwz;
    procedure gbra (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzxw;
    procedure gbar (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames yzwx;
    procedure garb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywxz;
    procedure gabr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames ywzx;
    procedure brga (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxyw;
    procedure brag (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zxwy;
    procedure bgra (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zyxw;
    procedure bgar (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zywx;
    procedure barg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwxy;
    procedure bagr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames zwyx;
    procedure argb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxyz;
    procedure arbg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wxzy;
    procedure agrb (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyxz;
    procedure agbr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wyzx;
    procedure abrg (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzxy;
    procedure abgr (vec1 : in out Vkm_GenType;
                    vec2 : in     Vkm_GenType) renames wzyx;
                   
                   
    ----------------------------------------------------------------------------
    -- Generic Vector Operations
    ----------------------------------------------------------------------------
    -- The following notation is used to descript the signatures of the generic
    -- functions:
    --     IV - A function has an input vector parameter
    --     OV - A function has an output vector parameter
    --     RV - A function returns a vector
    --     IS - A function has an input scalar paramter.
    --
    -- The order of these symbols indicates the expected order that these parameter
    -- are used in function passed in as a generic.
    ----------------------------------------------------------------------------
    -- @brief
    -- Apply function with pattern 'Func(IV,IV) return RV'.
    --
    ----------------------------------------------------------------------------
    generic
        with function Func(Left, Right : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IV_RV(Left, Right : in     Vkm_GenType) return Vkm_GenType;
    generic
        with function Func(Left, Right : in     Base_Type) return Base_Type;
    function Apply_Func_IS_IV_RV(Left  : in     Base_Type;
                                      Right : in     Vkm_GenType) return Vkm_GenType;
    generic
        with function Func(Left, Right : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IS_RV(Left  : in     Vkm_GenType;
                                 Right : in     Base_Type  ) return Vkm_GenType;
    generic
        with function Func(A : in     Base_Type) return Base_Type;
    function Apply_Func_IV_RV(A : in     Vkm_GenType) return Vkm_GenType;
    generic
        with function Func(IS1 : in     Base_Type;
                           OS1 :    out Base_Type) return Base_Type;
    function Apply_Func_IV_OV_RV(IV1 : in     Vkm_GenType;
                                 OV1 :    out Vkm_GenType) return Vkm_GenType;
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IV_IV_RV(IV1, IV2, IV3 : in     Vkm_GenType) return Vkm_GenType;
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IV_IS_RV(IV1, IV2 : in     Vkm_GenType;
                                    IS1      : in     Base_Type) return Vkm_GenType;
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IS_IS_RV(IV1      : in     Vkm_GenType;
                                    IS1, IS2 : in     Base_Type) return Vkm_GenType;


end Vulkan.Math.GenType;
