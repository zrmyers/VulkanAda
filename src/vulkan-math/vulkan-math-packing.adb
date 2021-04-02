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
with Vulkan.Math.Integers;
with Vulkan.Math.Common;
with Vulkan.Math.GenFType;
with Ada.Unchecked_Conversion;

use Vulkan.Math.Integers;
use Vulkan.Math.Common;
use Vulkan.Math.GenFType;

package body Vulkan.Math.Packing is

    -- A short value.
    type Half_Float_Unused_Bits is mod 2 ** 16;

    -- A sign bit.
    type Sign_Bit is mod 2 ** 1;

    -- Single float exponent MSB bits
    type Single_Float_Exponent_Msb is mod 2 ** 3;

    -- Single float exponent LSB bits
    type Single_Float_Exponent_Lsb is mod 2 ** 5;

    -- Single float mantissa MSB bits
    type Single_Float_Mantissa_Msb is mod 2 ** 10;

    -- Single float mantissa LSB bits
    type Single_Float_Mantissa_Lsb is mod 2 ** 13;

    -- The layout of a single-precision floating point number.
    type Single_Float_Bits is record
        sign         : Sign_Bit;
        exponent_msb : Single_Float_Exponent_Msb;
        exponent_lsb : Single_Float_Exponent_Lsb;
        mantissa_msb : Single_Float_Mantissa_Msb;
        mantissa_lsb : Single_Float_Mantissa_Lsb;
    end record;

    -- The bit positions to use for each field of the record.
    for Single_Float_Bits use record
        sign         at 0 range 31 .. 31;
        exponent_msb at 0 range 28 .. 30;
        exponent_lsb at 0 range 23 .. 27;
        mantissa_msb at 0 range 13 .. 22;
        mantissa_lsb at 0 range  0 .. 12;
    end record;

    -- The size of a single precision float.
    for Single_Float_Bits'Size use 32;

    -- The layout of a half-precision floating point number.
    type Vkm_Half_Float_Bits is record
        unused   : Half_Float_Unused_Bits;
        sign     : Sign_Bit;
        exponent : Single_Float_Exponent_Lsb;
        mantissa : Single_Float_Mantissa_Msb;
    end record;

    -- The bit positions to use for each field of the record.
    for Vkm_Half_Float_Bits use record
        unused   at 0 range 16 .. 31;
        sign     at 0 range 15 .. 15;
        exponent at 0 range 10 .. 14;
        mantissa at 0 range  0 ..  9;
    end record;

    -- The size of a half-precision float.
    for Vkm_Half_Float_Bits'Size use 32;

    -- The layout of a packed double-precision floating point number.
    type Vkm_Double_Float_Bits is record
        msb : Vkm_Uint;
        lsb : Vkm_Uint;
    end record;

    -- The bit positions to use for each field of the record.
    for Vkm_Double_Float_Bits use record
        msb at 0 range 32 .. 63;
        lsb at 0 range  0 .. 31;
    end record;

    -- The size of a double-precision float.
    for Vkm_Double_Float_Bits'Size use 64;


    ----------------------------------------------------------------------------
    -- Unchecked Conversion Operations
    ----------------------------------------------------------------------------
    -- Unchecked conversion from 32-bit float to Single Float Bits.
    function Convert_Vkm_Float_To_Single_Float_Bits is new
        Ada.Unchecked_Conversion(Source => Vkm_Float, Target => Single_Float_Bits);


    -- Unchecked conversion to 32-bit float from Single Float Bits.
    function Convert_Single_Float_Bits_To_Vkm_Float is new
        Ada.Unchecked_Conversion(Source => Single_Float_Bits, Target => Vkm_Float);


    -- Unchecked conversion from Half_Float_Bits to unsigned integer.
    function Convert_Vkm_Half_Float_Bits_To_Vkm_Uint is new
        Ada.Unchecked_Conversion(Source => Vkm_Half_Float_Bits, Target => Vkm_Uint);


    -- Unchecked conversion from unsigned integer to Vkm_Half_Float_Bits.
    function Convert_Vkm_Uint_To_Half_Float_Bits is new
        Ada.Unchecked_Conversion(Source => Vkm_Uint, Target => Vkm_Half_Float_Bits);


    -- Unchecked conversion from Vkm_Double_Float_Bits to Vkm_Double.
    function Convert_Vkm_Double_Float_Bits_To_Vkm_Double is new
        Ada.Unchecked_Conversion(Source => Vkm_Double_Float_Bits, Target => Vkm_Double);


    -- Unchecked conversion from Vkm_Double_Float_Bits to Vkm_Double.
    function Convert_Vkm_Double_To_Vkm_Double_Float_Bits is new
        Ada.Unchecked_Conversion(Source => Vkm_Double, Target => Vkm_Double_Float_Bits);


    ----------------------------------------------------------------------------
    -- Local Operation Declarations
    ----------------------------------------------------------------------------
    -- @summary
    -- Convert a single-precision floating point number to a half-precision
    -- floating point number.
    --
    -- @description
    -- Convert a single-precision floating point number to a half-precision
    -- floating point number.
    --
    -- @param value
    -- The Vkm_Float to convert to bits for a half-precision floating point number.
    --
    -- @return
    -- The bits for a half-precision floating point number.
    ----------------------------------------------------------------------------
    function Convert_Single_To_Half(
        value : in     Vkm_Float) return Vkm_Uint;


    ----------------------------------------------------------------------------
    -- @summary
    -- Convert a half-precision floating point number to a single-precision
    -- floating point number.
    --
    -- @description
    -- Convert a half-precision floating point number to a single-precision
    -- floating point number.
    --
    -- @param value
    -- The bits for a half-precision floating point number to convert to a
    -- single-precision floating point number.
    --
    -- @return
    -- The bits for a half-precision floating point number.
    ----------------------------------------------------------------------------
    function Convert_Half_To_Single(
        value : in     Vkm_Uint) return Vkm_Float;


    ----------------------------------------------------------------------------
    -- Operations
    ----------------------------------------------------------------------------


    function Pack_Unsigned_Normalized_2x16(
        vector : in     Vkm_Vec2) return Vkm_Uint is

        converted : constant Vkm_Vec2 := Round(Clamp(vector, 0.0, 1.0) * 65535.0);
        packed : Vkm_Uint := 0;
    begin

        packed := Bitfield_Insert(packed, To_Vkm_Uint(converted.x),  0, 16);
        packed := Bitfield_Insert(packed, To_Vkm_Uint(converted.y), 16, 16);

        return packed;

    end Pack_Unsigned_Normalized_2x16;


    ----------------------------------------------------------------------------



    function Pack_Signed_Normalized_2x16(
        vector : in     Vkm_Vec2) return Vkm_Uint is

        converted : constant Vkm_Vec2 := Round(Clamp(vector, -1.0, 1.0) * 32767.0);
        packed : Vkm_Int := 0;
    begin

        packed := Bitfield_Insert(packed, To_Vkm_Int(converted.x),  0, 16);
        packed := Bitfield_Insert(packed, To_Vkm_Int(converted.y), 16, 16);

        return To_Vkm_Uint(packed);

    end Pack_Signed_Normalized_2x16;


    ----------------------------------------------------------------------------


    function Pack_Unsigned_Normalized_4x8(
        vector : in     Vkm_Vec4) return Vkm_Uint is

        converted : constant Vkm_Vec4 := Round(Clamp(vector, 0.0, 1.0) * 255.0);
        packed : Vkm_Uint := 0;
    begin

        packed := Bitfield_Insert(packed, To_Vkm_Uint(converted.x),  0, 8);
        packed := Bitfield_Insert(packed, To_Vkm_Uint(converted.y),  8, 8);
        packed := Bitfield_Insert(packed, To_Vkm_Uint(converted.z), 16, 8);
        packed := Bitfield_Insert(packed, To_Vkm_Uint(converted.w), 24, 8);

        return packed;

    end Pack_Unsigned_Normalized_4x8;


    ----------------------------------------------------------------------------



    function Pack_Signed_Normalized_4x8(
        vector : in     Vkm_Vec4) return Vkm_Uint is

        converted : constant Vkm_Vec4 := Round(Clamp(vector, -1.0, 1.0) * 127.0);
        packed : Vkm_Int := 0;
    begin

        packed := Bitfield_Insert(packed, To_Vkm_Int(converted.x),  0, 8);
        packed := Bitfield_Insert(packed, To_Vkm_Int(converted.y),  8, 8);
        packed := Bitfield_Insert(packed, To_Vkm_Int(converted.z), 16, 8);
        packed := Bitfield_Insert(packed, To_Vkm_Int(converted.w), 24, 8);

        return To_Vkm_Uint(packed);

    end Pack_Signed_Normalized_4x8;


    ----------------------------------------------------------------------------



    function Unpack_Unsigned_Normalized_2x16(
        packed : in     Vkm_Uint) return Vkm_Vec2 is

        unpacked : Vkm_Vec2 := Make_Vec2;

    begin

        unpacked.x(To_Vkm_Float(Bitfield_Extract(packed,  0, 16)));
        unpacked.y(To_Vkm_Float(Bitfield_Extract(packed, 16, 16)));

        return unpacked / 65535.0;

    end Unpack_Unsigned_Normalized_2x16;


    ----------------------------------------------------------------------------



    function Unpack_Signed_Normalized_2x16(
        packed : in     Vkm_Uint) return Vkm_Vec2 is

        unpacked : Vkm_Vec2 := Make_Vec2;

    begin

        unpacked.x(To_Vkm_Float(Bitfield_Extract(To_Vkm_Int(packed),  0, 16)));
        unpacked.y(To_Vkm_Float(Bitfield_Extract(To_Vkm_Int(packed), 16, 16)));

        return Clamp(unpacked / 32767.0, -1.0, 1.0);

    end Unpack_Signed_Normalized_2x16;


    ----------------------------------------------------------------------------



    function Unpack_Unsigned_Normalized_4x8(
        packed : in     Vkm_Uint) return Vkm_Vec4 is

        unpacked : Vkm_Vec4 := Make_Vec4;

    begin

        unpacked.x(To_Vkm_Float(Bitfield_Extract(packed,  0, 8)));
        unpacked.y(To_Vkm_Float(Bitfield_Extract(packed,  8, 8)));
        unpacked.z(To_Vkm_Float(Bitfield_Extract(packed, 16, 8)));
        unpacked.w(To_Vkm_Float(Bitfield_Extract(packed, 24, 8)));

        return unpacked / 255.0;

    end Unpack_Unsigned_Normalized_4x8;


    ----------------------------------------------------------------------------



    function Unpack_Signed_Normalized_4x8(
        packed : in     Vkm_Uint) return Vkm_Vec4 is

        unpacked : Vkm_Vec4 := Make_Vec4;

    begin

        unpacked.x(To_Vkm_Float(Bitfield_Extract(To_Vkm_Int(packed),  0, 8)))
                .y(To_Vkm_Float(Bitfield_Extract(To_Vkm_Int(packed),  8, 8)))
                .z(To_Vkm_Float(Bitfield_Extract(To_Vkm_Int(packed), 16, 8)))
                .w(To_Vkm_Float(Bitfield_Extract(To_Vkm_Int(packed), 24, 8)));

        return Clamp( unpacked / 127.0, -1.0, 1.0);

    end Unpack_Signed_Normalized_4x8;


    ----------------------------------------------------------------------------



    function Pack_Half_2x16(
        vector : in     Vkm_Vec2) return Vkm_Uint is

        packed : Vkm_Uint := 0;

    begin

        packed := Bitfield_Insert(packed, Convert_Single_To_Half(vector.x),  0, 16);
        packed := Bitfield_Insert(packed, Convert_Single_To_Half(vector.y), 16, 16);

        return packed;
    end Pack_Half_2x16;


    ----------------------------------------------------------------------------



    function Unpack_Half_2x16(
        packed : in     Vkm_Uint) return Vkm_Vec2 is

        unpacked : Vkm_Vec2 := Make_Vec2;

    begin

        unpacked.x(Convert_Half_To_Single(Bitfield_Extract(packed,  0, 16)))
                .y(Convert_Half_To_Single(Bitfield_Extract(packed, 16, 16)));

        return unpacked;
    end Unpack_Half_2x16;


    ----------------------------------------------------------------------------



    function Pack_Double_2x32(
        vector : in     Vkm_Uvec2) return Vkm_Double is

        double_float_bits : constant Vkm_Double_Float_Bits :=
            (lsb => vector.x,
             msb => vector.y);

    begin

        return Convert_Vkm_Double_Float_Bits_To_Vkm_Double(double_float_bits);

    end Pack_Double_2x32;


    ----------------------------------------------------------------------------


    function Unpack_Double_2x32(
        packed : in     Vkm_Double) return Vkm_Uvec2 is

        double_float_bits : constant Vkm_Double_Float_Bits :=
            Convert_Vkm_Double_To_Vkm_Double_Float_Bits(packed);
        unpacked : Vkm_Uvec2 := Make_Uvec2;

    begin

        unpacked.x(double_float_bits.lsb)
                .y(double_float_bits.msb);

        return unpacked;
    end Unpack_Double_2x32;


    ----------------------------------------------------------------------------
    -- Local Operation Definitions
    ----------------------------------------------------------------------------


    function Convert_Single_To_Half(
        value : in     Vkm_Float) return Vkm_Uint is

        float_bits : constant Single_Float_Bits :=
            Convert_Vkm_Float_To_Single_Float_Bits(value);
        half_float_bits : constant Vkm_Half_Float_Bits :=
            (unused   => 0,
             sign     => float_bits.sign,
             exponent => float_bits.exponent_lsb,
             mantissa => float_bits.mantissa_msb);
    begin
        return Convert_Vkm_Half_Float_Bits_To_Vkm_Uint(half_float_bits);
    end Convert_Single_To_Half;


    ----------------------------------------------------------------------------


    function Convert_Half_To_Single(
        value : in     Vkm_Uint) return Vkm_Float is

        half_float_bits : constant Vkm_Half_Float_Bits :=
            Convert_Vkm_Uint_To_Half_Float_Bits(value);
        float_bits : constant Single_Float_Bits :=
            (sign         => half_float_bits.sign,
             exponent_msb => 0,
             exponent_lsb => half_float_bits.exponent,
             mantissa_msb => half_float_bits.mantissa,
             mantissa_lsb => 0);
    begin
        return Convert_Single_Float_Bits_To_Vkm_Float(float_bits);
    end Convert_Half_To_Single;


end Vulkan.Math.Packing;
