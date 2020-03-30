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
package Vulkan.Math.GenType is
    pragma Preelaborate;
    pragma Pure;

    type Vkm_Vector is array(Vkm_Indices range <>) of aliased Base_Type;
    pragma Convention(C,Vkm_Vector);


    type Vkm_GenType(Last_Index : Vkm_Indices) is tagged
        record
            data : Vkm_Vector(Vkm_Indices'First .. Last_Index);
        end record;

    ----------------------------------------------------------------------------
    -- Operations on Vkm_GenType
    ----------------------------------------------------------------------------
    function Length (A : in     Vkm_GenType) return Vkm_Length;

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


    ----------------------------------------------------------------------------
    -- @brief
    -- Apply function with pattern 'Func(IS,IV) return RV'.
    --
    ----------------------------------------------------------------------------
    generic
        with function Func(Left, Right : in     Base_Type) return Base_Type;
    function Apply_Func_IS_IV_RV(Left  : in     Base_Type;
                                      Right : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Apply function with pattern 'Func(IV,IS) return RV'.
    --
    ----------------------------------------------------------------------------
    generic
        with function Func(Left, Right : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IS_RV(Left  : in     Vkm_GenType;
                                      Right : in     Base_Type  ) return Vkm_GenType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Apply function with pattern 'Func(IV) return RV'
    --
    ----------------------------------------------------------------------------
    generic
        with function Func(A : in     Base_Type) return Base_Type;
    function Apply_Func_IV_RV(A : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Apply function with pattern 'Func(IV,OV) return RV'
    --
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1 : in     Base_Type;
                           OS1 :    out Base_Type) return Base_Type;
    function Apply_Func_IV_OV_RV(IV1 : in     Vkm_GenType;
                                 OV1 :    out Vkm_GenType) return Vkm_GenType;

    ----------------------------------------------------------------------------
    -- @brief
    -- Apply function with pattern 'Func(IV,IV,IV) return RV'
    --
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IV_IV_RV(IV1, IV2, IV3 : in     Vkm_GenType) return Vkm_GenType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Apply function with pattern 'Func(IV,IV,SV) return RV'
    --
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IV_IS_RV(IV1, IV2 : in     Vkm_GenType;
                                    IS1      : in     Base_Type) return Vkm_GenType;


    ----------------------------------------------------------------------------
    -- @brief
    -- Apply function with pattern 'Func(IV,SV,SV) return RV'
    --
    ----------------------------------------------------------------------------
    generic
        with function Func(IS1, IS2, IS3 : in     Base_Type) return Base_Type;
    function Apply_Func_IV_IS_IS_RV(IV1      : in     Vkm_GenType;
                                    IS1, IS2 : in     Base_Type) return Vkm_GenType;


end Vulkan.Math.GenType;
