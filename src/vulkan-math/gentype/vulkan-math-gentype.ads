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
