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
with Interfaces.C;
with Ada.Numerics;

package Vulkan.Math is
    pragma Preelaborate;
    pragma Pure;
    
    ----------------------------------------------------------------------------
    -- Math Constants
    ----------------------------------------------------------------------------
    PI : constant := Ada.Numerics.Pi;
    E  : constant := Ada.Numerics.e;
    
    ----------------------------------------------------------------------------
    -- Math Scalar Types
    ----------------------------------------------------------------------------
    -- Boolean Type
    type Vkm_Bool is new Boolean;
    for Vkm_Bool use (False => 0,
                      True  => 1);
    for Vkm_Bool'Size use Interfaces.C.unsigned_char'Size;
    
    -- Unsigned Integer Type
    type Vkm_Uint is new Interfaces.C.unsigned;
    
    -- Integer Type
    type Vkm_Int is new Interfaces.C.int;
    
    -- Single-Precision Floating Point Type
    type Vkm_Float is new Interfaces.C.C_Float;
    
    -- Double-Precision Floating Point Type
    type Vkm_Double is new Interfaces.C.double;
    
    -- Maximum Dimmension for a vector or a row or column of a matrix.
    type Vkm_Length is new Integer range 1 .. 4;
    
    -- The set of indices allowed for use with any vector or matrix.
    type Vkm_Indices is new Integer range 0 .. 3;
    
    
    ----------------------------------------------------------------------------
    -- Conversion Functions
    ----------------------------------------------------------------------------
    function To_Indices (length : in Vkm_Length) return Vkm_Indices;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- The following operations convert various VKM Math types to Vkm_Bool
    -- math types.
    --
    -- If the value is not equal to zero, returns true. Otherwise returns false.
    --
    -- @param[in]     value The value to convert to Vkm_Bool.
    --
    -- @return The conversion to Vkm_Bool.
    ----------------------------------------------------------------------------
    function To_Bool (value : in     Vkm_Uint  ) return Vkm_Bool;
    function To_Bool (value : in     Vkm_Int   ) return Vkm_Bool;
    function To_Bool (value : in     Vkm_Float ) return Vkm_Bool;
    function To_Bool (value : in     Vkm_Double) return Vkm_Bool;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- The following operations convert various VKM Math types to Vkm_Uint
    -- math types.
    --
    -- Conversion from Vkm_Int preserves the bit pattern of the argument, causing
    -- the value of negative arguments to change.
    --
    -- Conversion from a negative floating point argument will yield and 
    -- undefined result exception.
    --
    -- @param[in]     value The value to convert to Vkm_Uint.
    --
    -- @return The conversion to Vkm_Bool.
    --
    -- @error 
    -- The following exceptions are thrown:
    --     UNDEFINED_RESULT
    ----------------------------------------------------------------------------
    function To_Uint (value : in     Vkm_Bool  ) return Vkm_Uint;
    function To_Uint (value : in     Vkm_Int   ) return Vkm_Uint;
    function To_Uint (value : in     Vkm_Float ) return Vkm_Uint;
    function To_Uint (value : in     Vkm_Double) return Vkm_Uint;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- The following operations convert various VKM Math types to Vkm_Int
    -- math types.
    --
    -- Conversion from Vkm_Int preserves the bit pattern of the argument, causing
    -- the value of negative arguments to change.
    --
    -- @param[in]     value The value to convert to Vkm_Int.
    --
    -- @return The conversion to Vkm_Bool.
    ----------------------------------------------------------------------------
    function To_Int (value : in     Vkm_Bool  ) return Vkm_Int;
    function To_Int (value : in     Vkm_Uint  ) return Vkm_Int;
    function To_Int (value : in     Vkm_Float ) return Vkm_Int;
    function To_Int (value : in     Vkm_Double) return Vkm_Int;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- The following operations convert various VKM Math types to Vkm_Float
    -- math types.
    --
    -- @param[in]     value The value to convert to Vkm_Float.
    --
    -- @return The conversion to Vkm_Float.
    ----------------------------------------------------------------------------
    function To_Float (value : in     Vkm_Bool  ) return Vkm_Float;
    function To_Float (value : in     Vkm_Uint  ) return Vkm_Float;
    function To_Float (value : in     Vkm_Int   ) return Vkm_Float;
    function To_Float (value : in     Vkm_Double) return Vkm_Float;
    
    
    ----------------------------------------------------------------------------
    -- @brief
    -- The following operations convert various VKM Math types to Vkm_Float
    -- math types.
    --
    -- @param[in]     value The value to convert to Vkm_Float.
    --
    -- @return The conversion to Vkm_Float.
    ----------------------------------------------------------------------------
    function To_Double (value : in     Vkm_Bool ) return Vkm_Double;
    function To_Double (value : in     Vkm_Uint ) return Vkm_Double;
    function To_Double (value : in     Vkm_Int  ) return Vkm_Double;
    function To_Double (value : in     Vkm_Float) return Vkm_Double;
 
end Vulkan.Math;
