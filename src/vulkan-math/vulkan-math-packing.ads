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
with Vulkan.Math.Vec2;
with Vulkan.Math.Vec4;
with Vulkan.Math.Uvec2;

use Vulkan.Math.Vec2;
use Vulkan.Math.Vec4;
use Vulkan.Math.Uvec2;

--------------------------------------------------------------------------------
--< @group Vulkan Math Functions
--------------------------------------------------------------------------------
--< @summary
--< This package provides GLSL Floating Point Packing and Unpacking functions.
--<
--< @description
--< All floating point pack and unpack functions op
--------------------------------------------------------------------------------
package Vulkan.Math.Packing is
    pragma Preelaborate;
    pragma Pure;


    ----------------------------------------------------------------------------
    -- Normalized Vector Packing and Unpacking Functions
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation packs a normalized Vkm_Vec2 with unsigned components into
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the unsigned normalized input Vkm_Vec2 is packed into
    --< an 16-bit bitfield of an unsigned integer.
    --<
    --< The following conversion function is used to transform each floating
    --< point component into an 16-bit bitfield, where c is a component of the vector:
    --<
    --<     uint16_c := round( clamp ( c, 0, 1) * 65535.0)
    --<
    --< The packed vector is formatted as follows in the unsigned integer:
    --<     bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |
    --<     component |      y          |        x      |
    --<
    --< @param vector
    --< The normalized Vkm_Vec2 value to pack into an unsigned integer.
    --<
    --< @return
    --< The unsigned integer value.
    ----------------------------------------------------------------------------
    function Pack_Unsigned_Normalized_2x16(
        vector : in     Vkm_Vec2) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation packs a normalized Vkm_Vec2 with signed components into
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the signed normalized input Vkm_Vec2 is packed into
    --< an 16-bit bitfield of an unsigned integer.
    --<
    --< The following conversion function is used to transform each floating
    --< point component into an 16-bit bitfield, where c is a component of the vector:
    --<
    --<     int16_c := round( clamp ( c, -1, 1) * 32767.0)
    --<
    --< The packed vector is formatted as follows in the unsigned integer:
    --<     bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |
    --<     component |      y          |        x      |
    --<
    --< @param vector
    --< The normalized Vkm_Vec2 value to pack into an unsigned integer.
    --<
    --< @return
    --< The unsigned integer value.
    ----------------------------------------------------------------------------
    function Pack_Signed_Normalized_2x16(
        vector : in     Vkm_Vec2) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation packs a normalized Vkm_Vec4 with unsigned components into
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the unsigned normalized input Vkm_Vec4 is packed into
    --< an 8-bit bitfield of an unsigned integer.
    --<
    --< The following conversion function is used to transform each floating
    --< point component into an 8-bit bitfield, where c is a component of the vector:
    --<
    --<     uint8_c := round( clamp ( c, 0, 1) * 255.0)
    --<
    --< The packed vector is formatted as follows in the unsigned integer:
    --<     bits      | 31 ... 24 | 23 ... 16 | 15 ... 8 | 7 ... 0 |
    --<     component |     w     |     z     |     y    |    x    |
    --<
    --< @param vector
    --< The normalized Vkm_Vec4 value to pack into an unsigned integer.
    --<
    --< @return
    --< The unsigned integer value.
    ----------------------------------------------------------------------------
    function Pack_Unsigned_Normalized_4x8(
        vector : in     Vkm_Vec4) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation packs a normalized Vkm_Vec4 with signed components into
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the signed normalized input Vkm_Vec4 is packed into
    --< an 8-bit bitfield of an unsigned integer.
    --<
    --< The following conversion function is used to transform each floating
    --< point component into an 8-bit bitfield, where c is a component of the vector:
    --<
    --<     int8_c := round( clamp ( c, -1, 1) * 127.0)
    --<
    --< The packed vector is formatted as follows in the unsigned integer:
    --<     bits      | 31 ... 24 | 23 ... 16 | 15 ... 8 | 7 ... 0 |
    --<     component |     w     |     z     |     y    |    x    |
    --<
    --< @param vector
    --< The normalized Vkm_Vec4 value to pack into an unsigned integer.
    --<
    --< @return
    --< The unsigned integer value.
    ----------------------------------------------------------------------------
    function Pack_Signed_Normalized_4x8(
        vector : in     Vkm_Vec4) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation unpacks a normalized Vkm_Vec2 with unsigned components from
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the unsigned normalized output Vkm_Vec2 is unpacked from
    --< a 16-bit bitfield of an unsigned integer.
    --<
    --< The unpacked vector is extracted as follows from the unsigned integer:
    --<     bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |
    --<     component |      y          |        x      |
    --<
    --< The following conversion function is used to transform each 16-bit 
    --< bitfield into a floating point value, where c is a component of the vector,
    --< and uint16_c is the 16-bit packed component:
    --<
    --<     c := uint16_c / 65535.0
    --<
    --< @param packed
    --< The unsigned integer that contains the packed Vkm_Vec2.
    --<
    --< @return
    --< The unpacked signed normalized Vkm_Vec2 value.
    ----------------------------------------------------------------------------
    function Unpack_Unsigned_Normalized_2x16(
        packed : in     Vkm_Uint) return Vkm_Vec2;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation unpacks a normalized Vkm_Vec2 with signed components from
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the signed normalized output Vkm_Vec2 is unpacked from
    --< a 16-bit bitfield of an unsigned integer.
    --<
    --< The unpacked vector is extracted as follows from the unsigned integer:
    --<     bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |
    --<     component |      y          |        x      |
    --<
    --< The following conversion function is used to transform each 16-bit 
    --< bitfield into a floating point value, where c is a component of the vector,
    --< and uint16_c is the 16-bit packed component:
    --<
    --<     c := clamp(uint16_c / 32767.0, -1, 1)
    --<
    --< @param packed
    --< The unsigned integer that contains the packed Vkm_Vec2.
    --<
    --< @return
    --< The unpacked signed normalized Vkm_Vec2 value.
    ----------------------------------------------------------------------------
    function Unpack_Signed_Normalized_2x16(
        packed : in     Vkm_Uint) return Vkm_Vec2;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation unpacks a normalized Vkm_Vec4 with unsigned components from
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the unsigned normalized output Vkm_Vec4 is unpacked from
    --< an 8-bit bitfield of an unsigned integer.
    --<
    --< The unpacked vector is extracted as follows from the unsigned integer:
    --<     bits      | 31 ... 24 | 23 ... 16 | 15 ... 8 | 7 ... 0 |
    --<     component |     w     |     z     |     y    |    x    |
    --<
    --< The following conversion function is used to transform each 8-bit 
    --< bitfield into a floating point value, where c is a component of the vector,
    --< and uint8_c is the 8-bit packed component:
    --<
    --<     c := uint8_c / 256.0
    --<
    --< @param packed
    --< The unsigned integer that contains the packed Vkm_Vec4.
    --<
    --< @return
    --< The unpacked unsigned normalized Vkm_Vec4 value.
    ----------------------------------------------------------------------------
    function Unpack_Unsigned_Normalized_4x8(
        packed : in     Vkm_Uint) return Vkm_Vec4;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation unpacks a normalized Vkm_Vec4 with signed components from
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the signed normalized output Vkm_Vec4 is unpacked from
    --< an 8-bit bitfield of an unsigned integer.
    --<
    --< The unpacked vector is extracted as follows from the unsigned integer:
    --<     bits      | 31 ... 24 | 23 ... 16 | 15 ... 8 | 7 ... 0 |
    --<     component |     w     |     z     |     y    |    x    |
    --<
    --< The following conversion function is used to transform each 8-bit 
    --< bitfield into a floating point value, where c is a component of the vector,
    --< and uint8_c is the 8-bit packed component:
    --<
    --<     c := clamp(uint8_c / 127.0, -1, 1)
    --<
    --< @param packed
    --< The unsigned integer that contains the packed Vkm_Vec4.
    --<
    --< @return
    --< The unpacked signed normalized Vkm_Vec4 value.
    ----------------------------------------------------------------------------
    function Unpack_Signed_Normalized_4x8(
        packed : in     Vkm_Uint) return Vkm_Vec4;


    ----------------------------------------------------------------------------
    -- Half-Float packing and unpacking functions
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation packs components of a Vkm_Vec2 as half-floats into
    --< a 32-bit unsigned integer.
    --<
    --< @description
    --< Each component of the Vkm_Vec2 is converted to a half-precision floating
    --< point number and then packed into a 16-bit field of an unsigned integer.
    --<
    --< The floating point representations are shown below for reference:
    --<
    --<     bits         | Sign | Exponent | Significand |
    --<     Half-Float   | 15   | 14 .. 10 |   9 .. 0    |
    --<     Single-Float | 31   | 30 .. 23 |  22 .. 0    |
    --<
    --< Conversion is performed by copying the least significant bits of the fields
    --< of the single-precision floating point number to the corresponding fields
    --< of the half-precision floating point number.
    --<
    --< The vector is packed as follows into the unsigned integer:
    --<     bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |
    --<     component |      y          |        x      |
    --<
    --< @param vector
    --< The Vkm_Vec2 vector that is packed into the Vkm_Uint.
    --<
    --< @return
    --< The Vkm_Uint that contains the packed vector.
    ----------------------------------------------------------------------------
    function Pack_Half_2x16(
        vector : in     Vkm_Vec2) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation upacks a Vkm_Vec2 vector from an unsigned integer.
    --<
    --< @description
    --< Each component of the Vkm_Vec2 is converted from a half-precision floating
    --< point number after being unpacked from a 16-bit field of an unsigned integer.
    --<
    --< The packed vector is extracted as follows from the unsigned integer:
    --<     bits      | 31 30 ... 17 16 | 15 14 ... 1 0 |
    --<     component |      y          |        x      |
    --<
    --< The floating point representations are shown below for reference:
    --<
    --<     Fields       | Sign | Exponent | Significand |
    --<     Half-Float   | 15   | 14 .. 10 |   9 .. 0    |
    --<     Single-Float | 31   | 30 .. 23 |  22 .. 0    |
    --<
    --< Conversion is performed by copying the fields of the half-precision 
    --< floating point number to the least significant bits of the corresponding 
    --< fields of the single-precision floating point number.
    --<
    --< @param packed
    --< The Vkm_Uint that contains the packed vector.
    --<
    --< @return
    --< The unpacked Vkm_Vec2.
    ----------------------------------------------------------------------------
    function Unpack_Half_2x16(
        packed : in     Vkm_Uint) return Vkm_Vec2;


    ----------------------------------------------------------------------------
    -- Douple packing and unpacking functions
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation packs a Vkm_Uvec2 into a 64-bit Vkm_Double value.
    --<
    --< @description
    --< Each component of the Vkm_Uvec2 input is packed into a 32-bit bitfield 
    --< of a Vkm_Double.
    --<
    --< The Vkm_Uvec2 is packed into the Vkm_Double as follows:
    --<     bits      | 63 62 ... 33 32 | 31 30 ... 1 0 |
    --<     component |      y          |        x      |
    --<
    --< @param vector
    --< The Vkm_Uvec2 vector that is packed into the double.
    --<
    --< @return
    --< The Vkm_Double that contains the packed Vkm_Uvec2 value.
    ----------------------------------------------------------------------------
    function Pack_Double_2x32(
        vector : in     Vkm_Uvec2) return Vkm_Double;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation unpacks a Vkm_Uvec2 from a 64-bit Vkm_Double value.
    --<
    --< @description
    --< Each component of the Vkm_Uvec2 output is unpacked from a 32-bit bitfield 
    --< of a Vkm_Double.
    --<
    --< The Vkm_Uvec2 is unpacked from the Vkm_Double as follows:
    --<     bits      | 63 62 ... 33 32 | 31 30 ... 1 0 |
    --<     component |      y          |        x      |
    --<
    --< @param packed
    --< The Vkm_Double that contains the packed Vkm_Uvec2.
    --<
    --< @return
    --< The Vkm_Uvec2 unpacked from the Vkm_Double.
    ----------------------------------------------------------------------------
    function Unpack_Double_2x32(
        packed : in     Vkm_Double) return Vkm_Uvec2;


end Vulkan.Math.Packing;
