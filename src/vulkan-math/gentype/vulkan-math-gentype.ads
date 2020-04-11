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

    -- A pointer to a value of type Base_Type
    type Vkm_Access_Component(Data: not null access Base_Type) is null record
        with Implicit_Dereference => Data;

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

    ----------------------------------------------------------------------------
    -- Access Components of the Vector
    ----------------------------------------------------------------------------
    -- 1 D -> 1 D, X
    ----------------------------------------------------------------------------
    function X (Instance : in     Vkm_GenType) return Vkm_Access_Component is
        (Data => Instance.data(0)'Unrestricted_Access) with Inline;

    ----------------------------------------------------------------------------
    -- 1 D -> 1 D, R
    ----------------------------------------------------------------------------
    function R (Instance : in     Vkm_GenType) return Vkm_Access_Component renames X;

    ----------------------------------------------------------------------------
    -- 1 D -> 1 D, G
    ----------------------------------------------------------------------------
    function S (Instance : in     Vkm_GenType) return Vkm_Access_Component renames X;

    ----------------------------------------------------------------------------
    -- 2 D -> 1 D, XY
    ----------------------------------------------------------------------------
    function Y (Instance : in     Vkm_GenType) return Vkm_Access_Component is
        (Data => Instance.data(1)'Unrestricted_Access) with Inline;

    ----------------------------------------------------------------------------
    -- 2 D -> 1 D, RG
    ----------------------------------------------------------------------------
    function G (Instance : in     Vkm_GenType) return Vkm_Access_Component renames Y;

    ----------------------------------------------------------------------------
    -- 2 D -> 1 D, ST
    ----------------------------------------------------------------------------
    function T (Instance : in     Vkm_GenType) return Vkm_Access_Component renames Y;

    ----------------------------------------------------------------------------
    -- 3 D -> 1 D, XYZ
    ----------------------------------------------------------------------------
    function Z (Instance : in     Vkm_GenType) return Vkm_Access_Component is
        (Data => Instance.data(2)'Unrestricted_Access) with Inline;

    ----------------------------------------------------------------------------
    -- 3 D -> 1 D, RGB
    ----------------------------------------------------------------------------
    function B (Instance : in     Vkm_GenType) return Vkm_Access_Component renames Z;

    ----------------------------------------------------------------------------
    -- 3 D -> 1 D, STP
    ----------------------------------------------------------------------------
    function P (Instance : in     Vkm_GenType) return Vkm_Access_Component renames Z;

    ----------------------------------------------------------------------------
    -- 4 D -> 1 D, XYZW
    ----------------------------------------------------------------------------
    function W (Instance : in     Vkm_GenType) return Vkm_Access_Component is
        (Data => Instance.data(3)'Unrestricted_Access) with Inline;

    ----------------------------------------------------------------------------
    -- 4 D -> 1 D, RGBA
    ----------------------------------------------------------------------------
    function A (Instance : in     Vkm_GenType) return Vkm_Access_Component renames W;

    ----------------------------------------------------------------------------
    -- 4 D -> 1 D, STPQ
    ----------------------------------------------------------------------------
    function Q (Instance : in     Vkm_GenType) return Vkm_Access_Component renames W;

    ----------------------------------------------------------------------------
    -- 1 D -> 2 D, X
    ----------------------------------------------------------------------------
    function XX (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.x, Instance.x)) with Inline;

    ----------------------------------------------------------------------------
    -- 1 D -> 2 D, R
    ----------------------------------------------------------------------------
    function RR (Instance : in     Vkm_GenType) return Vkm_GenType renames XX;

    ----------------------------------------------------------------------------
    -- 1 D -> 2 D, S
    ----------------------------------------------------------------------------
    function SS (Instance : in     Vkm_GenType) return Vkm_GenType renames XX;

    ----------------------------------------------------------------------------
    -- 2 D -> 2 D, XY
    ----------------------------------------------------------------------------
    function YY (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.y, Instance.y)) with Inline;
    function YX (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.y, Instance.x)) with Inline;
    function XY (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.x, Instance.y)) with Inline;

    ----------------------------------------------------------------------------
    -- 2 D -> 2 D, RG
    ----------------------------------------------------------------------------
    function GG (Instance : in     Vkm_GenType) return Vkm_GenType renames YY;
    function GR (Instance : in     Vkm_GenType) return Vkm_GenType renames YX;
    function RG (Instance : in     Vkm_GenType) return Vkm_GenType renames XY;

    ----------------------------------------------------------------------------
    -- 2 D -> 2 D, ST
    ----------------------------------------------------------------------------
    function TT (Instance : in     Vkm_GenType) return Vkm_GenType renames YY;
    function TS (Instance : in     Vkm_GenType) return Vkm_GenType renames YX;
    function ST (Instance : in     Vkm_GenType) return Vkm_GenType renames XY;

    ----------------------------------------------------------------------------
    -- 3 D -> 2 D, XYZ
    ----------------------------------------------------------------------------
    function ZZ (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.z, Instance.z)) with Inline;
    function ZX (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.z, Instance.x)) with Inline;
    function XZ (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.x, Instance.z)) with Inline;
    function ZY (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.z, Instance.y)) with Inline;
    function YZ (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.y, Instance.z)) with Inline;

    ----------------------------------------------------------------------------
    -- 3 D -> 2 D, RGB
    ----------------------------------------------------------------------------
    function BB (Instance : in     Vkm_GenType) return Vkm_GenType renames ZZ;
    function BR (Instance : in     Vkm_GenType) return Vkm_GenType renames ZX;
    function RB (Instance : in     Vkm_GenType) return Vkm_GenType renames XZ;
    function BG (Instance : in     Vkm_GenType) return Vkm_GenType renames ZY;
    function GB (Instance : in     Vkm_GenType) return Vkm_GenType renames YZ;

    ----------------------------------------------------------------------------
    -- 3 D -> 2 D, STP
    ----------------------------------------------------------------------------
    function PP (Instance : in     Vkm_GenType) return Vkm_GenType renames ZZ;
    function PS (Instance : in     Vkm_GenType) return Vkm_GenType renames ZX;
    function SP (Instance : in     Vkm_GenType) return Vkm_GenType renames XZ;
    function PT (Instance : in     Vkm_GenType) return Vkm_GenType renames ZY;
    function TP (Instance : in     Vkm_GenType) return Vkm_GenType renames YZ;

    ----------------------------------------------------------------------------
    -- 4 D to 2 D, XYZW
    ----------------------------------------------------------------------------
    function WW (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.w, Instance.w)) with Inline;
    function WX (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.w, Instance.x)) with Inline;
    function XW (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.x, Instance.w)) with Inline;
    function WY (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.w, Instance.y)) with Inline;
    function YW (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.y, Instance.w)) with Inline;
    function WZ (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.w, Instance.z)) with Inline;
    function ZW (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.z, Instance.w)) with Inline;

    ----------------------------------------------------------------------------
    -- 4 D to 2 D, RGBA
    ----------------------------------------------------------------------------
    function AA (Instance : in     Vkm_GenType) return Vkm_GenType renames WW;
    function AR (Instance : in     Vkm_GenType) return Vkm_GenType renames WX;
    function RA (Instance : in     Vkm_GenType) return Vkm_GenType renames XW;
    function AG (Instance : in     Vkm_GenType) return Vkm_GenType renames WY;
    function GA (Instance : in     Vkm_GenType) return Vkm_GenType renames YW;
    function AB (Instance : in     Vkm_GenType) return Vkm_GenType renames WZ;
    function BA (Instance : in     Vkm_GenType) return Vkm_GenType renames ZW;

    ----------------------------------------------------------------------------
    -- 4 D to 2 D, STPQ
    ----------------------------------------------------------------------------
    function QQ (Instance : in     Vkm_GenType) return Vkm_GenType renames WW;
    function QS (Instance : in     Vkm_GenType) return Vkm_GenType renames WX;
    function SQ (Instance : in     Vkm_GenType) return Vkm_GenType renames XW;
    function QT (Instance : in     Vkm_GenType) return Vkm_GenType renames WY;
    function TQ (Instance : in     Vkm_GenType) return Vkm_GenType renames YW;
    function QP (Instance : in     Vkm_GenType) return Vkm_GenType renames WZ;
    function PQ (Instance : in     Vkm_GenType) return Vkm_GenType renames ZW;

    ----------------------------------------------------------------------------
    -- 1 D -> 3 D, X
    ----------------------------------------------------------------------------
    function XXX (Instance : in     Vkm_GenType) return Vkm_GenType is
        (Make(Instance.x, Instance.x, Instance.x)) with Inline;

    ----------------------------------------------------------------------------
    -- 1 D -> 3 D, R
    ----------------------------------------------------------------------------
    function RRR (Instance : in     Vkm_GenType) return Vkm_GenType renames XXX;

    ----------------------------------------------------------------------------
    -- 1 D -> 3 D, S
    ----------------------------------------------------------------------------
    function SSS (Instance : in     Vkm_GenType) return Vkm_GenType renames XXX;



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
