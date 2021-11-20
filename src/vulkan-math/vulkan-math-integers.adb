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
with Ada.Unchecked_Conversion;
with Interfaces;

use Interfaces;

package body Vulkan.Math.Integers is

    -- 32-bit LSB mask.
    LSB32_MASK : constant := 16#00000000FFFFFFFF#;
    
    -- A 64-bit unsigned integer type.
    type Vkm_Uint64 is new Interfaces.Unsigned_64;

    -- A 64-bit signed integer type.
    type Vkm_Int64 is new Interfaces.Integer_64;
    
    -- A representation of a 32-bit integer as an array of 32 booleans.
    type Vkm_Bits32 is array (0 .. 31) of Boolean;
    for Vkm_Bits32'Component_Size use 1;
    for Vkm_Bits32'Size use 32;
    
    ----------------------------------------------------------------------------
    -- Local Operations
    ----------------------------------------------------------------------------
    -- Unchecked conversion from a 64-bit signed integer to a 64-bit unsigned 
    -- integer.
    function To_Vkm_Uint64 is new 
        Ada.Unchecked_Conversion(
            Source => Vkm_Int64, 
            Target => Vkm_Uint64);
        
    function To_Vkm_Bits32 is new
        Ada.Unchecked_Conversion(
            Source => Vkm_Uint,
            Target => Vkm_Bits32);
            
    function To_Vkm_Bits32 is new
        Ada.Unchecked_Conversion(
            Source => Vkm_Int,
            Target => Vkm_Bits32);
            
    function To_Vkm_Uint is new
        Ada.Unchecked_Conversion(
            Source => Vkm_Bits32,
            Target => Vkm_Uint);
            
    function To_Vkm_Int is new
        Ada.Unchecked_Conversion(
            Source => Vkm_Bits32,
            Target => Vkm_Int);
            
    ----------------------------------------------------------------------------
    -- Operations
    ----------------------------------------------------------------------------
    
    
    function Unsigned_Add_Carry(x, y  : in     Vkm_Uint;
                                carry :    out Vkm_Uint) return Vkm_Uint is
    begin    
        if (Vkm_Uint64(x) + Vkm_Uint64(y)) > Vkm_Uint64(Vkm_Uint'Last) then
            carry := 1;
        else
            carry := 0;
        end if;        
        return x + y;
    end Unsigned_Add_Carry;
    
    
    ----------------------------------------------------------------------------
    
    
    function Unsigned_Sub_Borrow(x, y   : in     Vkm_Uint;
                                 borrow :    out Vkm_Uint) return Vkm_Uint is
    begin
        borrow := (if x >= y then 0 else 1);
        return x - y;
    end Unsigned_Sub_Borrow;


    ----------------------------------------------------------------------------
 
 
    procedure Unsigned_Mul_Extended(x, y     : in     Vkm_Uint;
                                    msb, lsb :    out Vkm_Uint) is
        result : constant Vkm_Uint64 := Vkm_Uint64(x) * Vkm_Uint64(y);
    begin
        lsb := Vkm_Uint(LSB32_MASK and result);
        msb := Vkm_Uint(LSB32_MASK and Shift_Right(result, 32));
    end Unsigned_Mul_Extended;


    ----------------------------------------------------------------------------
    
    
    procedure Signed_Mul_Extended(x, y     : in     Vkm_Int;
                                  msb, lsb :    out Vkm_Int) is
        result : constant Vkm_Int64 := Vkm_Int64(x) * Vkm_Int64(y);
    begin
        lsb := To_Vkm_Int(Vkm_Uint(LSB32_MASK and To_Vkm_Uint64(result)));
        msb := To_Vkm_Int(Vkm_Uint(LSB32_MASK and Shift_Right(To_Vkm_Uint64(result), 32)));
    end Signed_Mul_Extended;
    

    ----------------------------------------------------------------------------
    
    
    function Bitfield_Extract(value, offset, bits : in     Vkm_Int) return Vkm_Int is
    
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result_bits : Vkm_Bits32 := (others => value_bits(Integer(offset + bits -1)));
    begin
        for bit_index in 0 .. bits - 1 loop
            result_bits(Integer(bit_index)) := value_bits(Integer(offset + bit_index));
        end loop;
        return To_Vkm_Int(result_bits);
    end Bitfield_Extract;


    ----------------------------------------------------------------------------
    
    
    function Bitfield_Extract(value        : in     Vkm_Uint;
                              offset, bits : in     Vkm_Int) return Vkm_Uint is
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result_bits : Vkm_Bits32 := (others => False);
    begin
        for bit_index in 0 .. bits - 1 loop
            result_bits(Integer(bit_index)) := value_bits(Integer(offset + bit_index));
        end loop;
        return To_Vkm_Uint(result_bits);
    end Bitfield_Extract;
    
    
    ----------------------------------------------------------------------------
    
    
    function Bitfield_Insert(base, insert, offset, bits : in     Vkm_Int) return Vkm_Int is
        base_bits : Vkm_Bits32 := To_Vkm_Bits32(base);
        insert_bits : constant Vkm_Bits32 := To_Vkm_Bits32(insert);
    begin
        for bit_index in 0 .. bits - 1 loop
            base_bits(Integer(offset + bit_index)) := insert_bits(Integer(bit_index));
        end loop;
        return To_Vkm_Int(base_bits);
    end Bitfield_Insert;
    
    
    ----------------------------------------------------------------------------
    
    
    function Bitfield_Insert(base, insert : in     Vkm_Uint;
                             offset, bits : in     Vkm_Int) return Vkm_Uint is
        base_bits : Vkm_Bits32 := To_Vkm_Bits32(base);
        insert_bits : constant Vkm_Bits32 := To_Vkm_Bits32(insert);
    begin
        for bit_index in 0 .. bits - 1 loop
            base_bits(Integer(offset + bit_index)) := insert_bits(Integer(bit_index));
        end loop;
        return To_Vkm_Uint(base_bits);
    end Bitfield_Insert;


    ----------------------------------------------------------------------------
    
    
    function Bitfield_Reverse(value : in     Vkm_Int) return Vkm_Int is
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result_bits : Vkm_Bits32 := (others => False);
    begin 
        for bit_index in 0 .. 31 loop
            result_bits(bit_index) := value_bits(31 - bit_index);
        end loop;
        return To_Vkm_Int(result_bits);
    end Bitfield_Reverse;


    ----------------------------------------------------------------------------
    
    
    function Bitfield_Reverse(value : in     Vkm_Uint) return Vkm_Uint is
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result_bits : Vkm_Bits32 := (others => False);
    begin 
        for bit_index in 0 .. 31 loop
            result_bits(bit_index) := value_bits(31 - bit_index);
        end loop;
        return To_Vkm_Uint(result_bits);
    end Bitfield_Reverse;
    
    
    ----------------------------------------------------------------------------
    
    
    function Bit_Count(value : in     Vkm_Int) return Vkm_Int is
        value_bits : constant Vkm_Bits32  := To_Vkm_Bits32(value);
        result : Vkm_Int := 0;
    begin
        for bit_index in 0 .. 31 loop
            result := result + (if value_bits(bit_index) then 1 else 0);
        end loop;
        return result;
    end Bit_Count;
    
    
    ----------------------------------------------------------------------------
    
    
    function Bit_Count(value : in     Vkm_Uint) return Vkm_Int is
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result : Vkm_Int := 0;
    begin
        for bit_index in 0 .. 31 loop
            result := result + (if value_bits(bit_index) then 1 else 0);
        end loop;
        return result;
    end Bit_Count;
    
    
    ----------------------------------------------------------------------------
    
    
    function Find_Lsb(value : in     Vkm_Int) return Vkm_Int is
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result : Vkm_Int := -1;
    begin
        for bit_index in 0 .. 31 loop
            if value_bits(bit_index) then
                result := Vkm_Int(bit_index);
            end if;
            exit when result /= -1;
        end loop;
        return result;
    end Find_Lsb;
    
    
    ----------------------------------------------------------------------------
    
    
    function Find_Lsb(value : in     Vkm_Uint) return Vkm_Int is
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result : Vkm_Int := -1;
    begin
        for bit_index in 0 .. 31 loop
            if value_bits(bit_index) then
                result := Vkm_Int(bit_index);
            end if;
            exit when result /= -1;
        end loop;
        return result;
    end Find_Lsb;
    
    
    ----------------------------------------------------------------------------
    
    
    function Find_Msb(value : in     Vkm_Int) return Vkm_Int is
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result : Vkm_Int := -1;
    begin
        for bit_index in reverse 0 .. 31 loop
            if not value_bits(bit_index) then
                result := Vkm_Int(bit_index);
            end if;
            exit when result /= -1;
        end loop;
        return result;
    end Find_Msb;
    
    
    ----------------------------------------------------------------------------
    
    
    function Find_Msb(value : in     Vkm_Uint) return Vkm_Int is
        value_bits : constant Vkm_Bits32 := To_Vkm_Bits32(value);
        result : Vkm_Int := -1;
    begin
        for bit_index in reverse 0 .. 31 loop
            if value_bits(bit_index) then
                result := Vkm_Int(bit_index);
            end if;
            exit when result /= -1;
        end loop;
        return result;
    end Find_Msb;
                             
end Vulkan.Math.Integers;
