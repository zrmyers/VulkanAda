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
with Vulkan.Math.GenIType;
with Vulkan.Math.GenUType;

-- Uses
use Vulkan.Math.GenUType;
use Vulkan.Math.GenIType;

--------------------------------------------------------------------------------
--< @group Vulkan Math Functions
--------------------------------------------------------------------------------
--< @summary
--< This package provides GLSL Integer Built-in functions.
--<
--< @description
--< All common functions operate component-wise.
--------------------------------------------------------------------------------
package Vulkan.Math.Integers is
    pragma Preelaborate;
    pragma Pure;
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Adds two unsigned integers modulo 32, returning the result and a carry.
    --<
    --< @description
    --< Adds two unsigned integers modulo 32. If the result is greater than or
    --< equal 2^32 the carry is 1. Otherwise the carry is 0.
    --<
    --< @param x
    --< One of the addition operands.
    --< 
    --< @param y
    --< One of the addition operands.
    --<
    --< @param carry
    --< The carry value from the addition operation.
    --<
    --< @return
    --< The result of x + y modulo 32.
    ----------------------------------------------------------------------------
    function Unsigned_Add_Carry(x, y  : in     Vkm_Uint;
                                carry :    out Vkm_Uint) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< Adds two unsigned integers modulo 32, returning the result and a carry.
    --<
    --< @description
    --< Applies Unsigned_Add_Carry() component-wise on three Vkm_GenUType vectors,
    --< returning the result of addition as a Vkm_GenUType vector.
    ----------------------------------------------------------------------------
    function Unsigned_Add_Carry is new GUT.Apply_Func_IV_IV_OV_RV(Unsigned_Add_Carry);
    
    ----------------------------------------------------------------------------
    --< @summary
    --< Subtracts two unsigned integers modulo 32, returning the result and a borrow.
    --<
    --< @description
    --< Subtracts two unsigned integers modulo 32. If x is less than y, the result
    --< is 2^32 plus the difference and a borrow of 1; Otherwise, the result is
    --< the difference and a borrow of 0.
    --<
    --< @param x
    --< The left subtraction operand.
    --< 
    --< @param y
    --< The right subtraction operand.
    --<
    --< @param borrow
    --< The borrow value from the subtraction operation.
    --<
    --< @return
    --< The result of x - y modulo 32.
    ----------------------------------------------------------------------------
    function Unsigned_Sub_Borrow(x, y   : in     Vkm_Uint;
                                 borrow :    out Vkm_Uint) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< Subtracts two unsigned integers modulo 32, returning the result and a borrow.
    --<
    --< @description
    --< Applies Unsigned_Sub_Borrow() component-wise on three Vkm_GenUType vectors,
    --< returning the result of addition as a Vkm_GenUType vector.
    ----------------------------------------------------------------------------
    function Unsigned_Sub_Borrow is new GUT.Apply_Func_IV_IV_OV_RV(Unsigned_Sub_Borrow);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation performs extended multiplication of two 32-bit unsigned
    --< integers.
    --<
    --< @description
    --< Multiplies two 32-bit unsigned integers returning two 32-bit unsigned 
    --< integers representing the most significant and least significant 32 bits 
    --< of the result of multiplication.
    --<
    --< @param x
    --< The left subtraction operand.
    --< 
    --< @param y
    --< The right subtraction operand.
    --<
    --< @param msb
    --< The 32 most significant bits of the resulting 64-bit unsigned integer.
    --<
    --< @param lsb
    --< The 32 least significant bits of the resulting 64-bit unsigned integer.
    ----------------------------------------------------------------------------
    procedure Unsigned_Mul_Extended(x, y     : in     Vkm_Uint;
                                    msb, lsb :    out Vkm_Uint);

    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation performs extended multiplication of two Vkm_GenUType vectors.
    --<
    --< @description
    --< Applies Unsigned_Mul_Extended() component-wise on two input Vkm_GenUType
    --< vectors and two output Vkm_GenUType vectors.
    ----------------------------------------------------------------------------
    procedure Unsigned_Mul_Extended is new GUT.Apply_Func_IV_IV_OV_OV(Unsigned_Mul_Extended);


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation performs extended multiplication of two 32-bit signed
    --< integers.
    --<
    --< @description
    --< Multiplies two 32-bit signed integers returning two 32-bit signed 
    --< integers representing the most significant and least significant 32 bits 
    --< of the result of multiplication.
    --<
    --< @param x
    --< The left subtraction operand.
    --< 
    --< @param y
    --< The right subtraction operand.
    --<
    --< @param msb
    --< The 32 most significant bits of the resulting 64-bit signed integer.
    --<
    --< @param lsb
    --< The 32 least significant bits of the resulting 64-bit signed integer.
    ----------------------------------------------------------------------------
    procedure Signed_Mul_Extended(x, y     : in     Vkm_Int;
                                  msb, lsb :    out Vkm_Int);

    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation performs extended multiplication of two 32-bit signed
    --< integers.
    --<
    --< @description
    --< Applies Unsigned_Mul_Extended() component-wise on two input Vkm_GenIType
    --< vectors and two output Vkm_GenIType vectors.
    ----------------------------------------------------------------------------
    procedure Signed_Mul_Extended is new GIT.Apply_Func_IV_IV_OV_OV(Signed_Mul_Extended);


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation extracts a bitfield from a 32-bit signed integer.
    --<
    --< @description
    --< Extract bits starting from offset to the number of bits in the bitfield.
    --<
    --< Let bN = bits + offset -1.
    --< Let b0 = offset.
    --<
    --< In general, the bitfield is extracted from the value the value as follows:
    --<
    --<     \     | MSB         | Bitfield          | LSB        |
    --<     bits  | 31 ... bN+1 | bN bN-1 ... b1 b0 | b0-1 ... 0 |
    --<
    --< The result is formatted as follows, where N = bits -1: 
    --<
    --<     \     |        MSB        |              Bitfield                | 
    --<     bit   | 31     ... N+1    | N       N-1        ...   1      0    | 
    --<     value | bN'Val ... bN'Val | bN'Val (bN-1)'Val  ... b1'Val b0'Val |
    --<
    --< @param value
    --< The value from which the bitfield is extracted.
    --< 
    --< @param offset
    --< The offset into the value from which the bitfield is extracted.
    --<
    --< @param bits
    --< The number of bits in the bitfield.
    --<
    --< @return
    --< The extracted value from the bitfield. The most significant bits are set
    --< to the signed bit of the bitfield.
    ----------------------------------------------------------------------------
    function Bitfield_Extract(value, offset, bits : in     Vkm_Int) return Vkm_Int;

    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation extracts a bitfield vector from a Vkm_GenIType vector.
    --<
    --< @description
    --< Applies Bitfield_Extract() component-wise on an input Vkm_GenIType
    --< vector and two Vkm_Int scalars, returning a Vkm_GenIType vector of the
    --< extracted bitfields.
    ----------------------------------------------------------------------------
    function Bitfield_Extract is new GIT.Apply_Func_IV_IS_IS_RV(Bitfield_Extract);


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation extracts a bitfield from a 32-bit unsigned integer.
    --<
    --< @description
    --< Extract bits starting from offset to the number of bits in the bitfield.
    --<
    --< Let bN = bits + offset -1.
    --< Let b0 = offset.
    --<
    --< In general, the bitfield is extracted from the value the value as follows:
    --<
    --<     \     | MSB         | Bitfield          | LSB        |
    --<     bits  | 31 ... bN+1 | bN bN-1 ... b1 b0 | b0-1 ... 0 |
    --<
    --< The result is formatted as follows, where N = bits -1: 
    --<
    --<     \     |        MSB        |              Bitfield                | 
    --<     bit   | 31     ... N+1    | N       N-1        ...   1      0    | 
    --<     value | 0      ... 0      | bN'Val (bN-1)'Val  ... b1'Val b0'Val |
    --<
    --< @param value
    --< The value from which the bitfield is extracted.
    --< 
    --< @param offset
    --< The offset into the value from which the bitfield is extracted.
    --<
    --< @param bits
    --< The number of bits in the bitfield.
    --<
    --< @return
    --< The extracted value from the bitfield. The most significant bits are set
    --< to 0.
    ----------------------------------------------------------------------------
    function Bitfield_Extract(value        : in     Vkm_Uint;
                              offset, bits : in     Vkm_Int) return Vkm_Uint;

    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation extracts a bitfield vector from a Vkm_GenUType vector.
    --<
    --< @description
    --< Applies Bitfield_Extract() component-wise on an input Vkm_GenUType
    --< vector and two Vkm_Int scalars, returning a Vkm_GenUType vector of the
    --< extracted bitfields.
    ----------------------------------------------------------------------------
    function Bitfield_Extract is new Apply_Func_IVU_ISI_ISI_RVU(Bitfield_Extract);


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation inserts a bitfield into a 32-bit signed integer.
    --<
    --< @description
    --< Insert bits starting from offset to the number of bits in the bitfield.
    --<
    --< Let bN = bits + offset -1.
    --< Let b0 = offset.
    --<
    --< In general, the 'bits' least significant bits of 'insert' are copied into
    --< base as follows:
    --<
    --<     \     | MSB         | Bitfield          | LSB        |
    --<     bits  | 31 ... bN+1 | bN bN-1 ... b1 b0 | b0-1 ... 0 |
    --<     value | base        |      insert       | base       |
    --<
    --< @param base
    --< The value into which the bitfield is inserted.
    --< 
    --< @param insert
    --< The value that is inserted into base.
    --<
    --< @param offset
    --< The offset into the base at which the bitfield is inserted.
    --<
    --< @param bits
    --< The number of bits in the bitfield.
    --<
    --< @return
    --< The value of base with the bitfield inserted.
    ----------------------------------------------------------------------------
    function Bitfield_Insert(base, insert, offset, bits : in     Vkm_Int) return Vkm_Int;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation inserts a bitfield vector into a Vkm_GenIType vector.
    --<
    --< @description
    --< Applies Bitfield_Insert() component-wise on two input Vkm_GenIType
    --< vectors and two Vkm_Int scalars, returning a Vkm_GenIType vector of the
    --< base vector with inserted bitfields.
    ----------------------------------------------------------------------------
    function Bitfield_Insert is new GIT.Apply_Func_IV_IV_IS_IS_RV(Bitfield_Insert);


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation inserts a bitfield into a 32-bit unsigned integer.
    --<
    --< @description
    --< Insert bits starting from offset to the number of bits in the bitfield.
    --<
    --< Let bN = bits + offset -1.
    --< Let b0 = offset.
    --<
    --< In general, the 'bits' least significant bits of 'insert' are copied into
    --< base as follows:
    --<
    --<     \     | MSB         | Bitfield          | LSB        |
    --<     bits  | 31 ... bN+1 | bN bN-1 ... b1 b0 | b0-1 ... 0 |
    --<     value | base        |      insert       | base       |
    --<
    --< @param base
    --< The value into which the bitfield is inserted.
    --< 
    --< @param insert
    --< The value that is inserted into base.
    --<
    --< @param offset
    --< The offset into the base at which the bitfield is inserted.
    --<
    --< @param bits
    --< The number of bits in the bitfield.
    --<
    --< @return
    --< The value of base with the bitfield inserted.
    ----------------------------------------------------------------------------
    function Bitfield_Insert(base, insert : in     Vkm_Uint;
                             offset, bits : in     Vkm_Int) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation inserts a bitfield vector into a Vkm_GenUType vector.
    --<
    --< @description
    --< Applies Bitfield_Insert() component-wise on two input Vkm_GenUType
    --< vectors and two Vkm_Int scalars, returning a Vkm_GenUType vector of the
    --< base vector with inserted bitfields.
    ----------------------------------------------------------------------------
    function Bitfield_Insert is new Apply_Func_IVU_IVU_ISI_ISI_RVU(Bitfield_Insert);


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation reverses the bits of a 32-bit signed integer.
    --<
    --< @description
    --< Reverse the bits of a 32-bit signed integer.
    --<
    --< Let vI be the value of bit at bit position I in the input value.
    --<
    --<     bit    | 31  30  ... 1   0   |
    --<     input  | v31 v30 ... v1  v0  |
    --<     output | v0  v1  ... v30 v31 |
    --<
    --< @param value
    --< The value which is to be reversed.
    --<
    --< @return
    --< The reversed value.
    ----------------------------------------------------------------------------
    function Bitfield_Reverse(value : in     Vkm_Int) return Vkm_Int;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation reverses the binary representation of the components of a
    --< Vkm_GenIType vector.
    --<
    --< @description
    --< Applies Bitfield_Reverse() component-wise on input Vkm_GenIType, 
    --< returning a Vkm_GenIType vector of the reversed components.
    ----------------------------------------------------------------------------
    function Bitfield_Reverse is new GIT.Apply_Func_IV_RV(Bitfield_Reverse);


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation reverses the bits of a 32-bit unsigned integer.
    --<
    --< @description
    --< Reverse the bits of a 32-bit unsigned integer.
    --<
    --< Let vI be the value of bit at bit position I in the input value.
    --<
    --<     bit    | 31  30  ... 1   0   |
    --<     input  | v31 v30 ... v1  v0  |
    --<     output | v0  v1  ... v30 v31 |
    --<
    --< @param value
    --< The value which is to be reversed.
    --<
    --< @return
    --< The reversed value.
    ----------------------------------------------------------------------------
    function Bitfield_Reverse(value : in     Vkm_Uint) return Vkm_Uint;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation reverses the binary representation of the components of a
    --< Vkm_GenUType vector.
    --<
    --< @description
    --< Applies Bitfield_Reverse() component-wise on input Vkm_GenUType, 
    --< returning a Vkm_GenUType vector of the reversed components.
    ----------------------------------------------------------------------------
    function Bitfield_Reverse is new GUT.Apply_Func_IV_RV(Bitfield_Reverse);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation counts the number of 1-bits in a 32-bit signed value.
    --<
    --< @description
    --< Count the 1's bits of a 32-bit signed integer.
    --<
    --< @param value
    --< The value for which 1's bits are counted.
    --<
    --< @return
    --< The number of 1's in value.
    ----------------------------------------------------------------------------
    function Bit_Count(value : in     Vkm_Int) return Vkm_Int;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation counts the number of 1-bits for each component of a 
    --< Vkm_GenIType vector.
    --<
    --< @description
    --< Applies Bit_Count() component-wise on input Vkm_GenIType, 
    --< returning a Vkm_GenIType vector of counts for each component.
    ----------------------------------------------------------------------------
    function Bit_Count is new GIT.Apply_Func_IV_RV(Bit_Count);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation counts the number of 1-bits in a 32-bit unsigned value.
    --<
    --< @description
    --< Count the 1's bits of a 32-bit unsigned integer.
    --<
    --< @param value
    --< The value for which 1's bits are counted.
    --<
    --< @return
    --< The number of 1's in value.
    ----------------------------------------------------------------------------
    function Bit_Count(value : in     Vkm_Uint) return Vkm_Int;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation counts the number of 1-bits for each component of a 
    --< Vkm_GenUType vector.
    --<
    --< @description
    --< Applies Bit_Count() component-wise on input Vkm_GenUType, 
    --< returning a Vkm_GenIType vector of counts for each component.
    ----------------------------------------------------------------------------
    function Bit_Count is new Apply_Func_IVU_RVI(Bit_Count);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation finds the least significant 1-bit in the 32-bit signed
    --< integer.
    --<
    --< @description
    --< Find the least significant bit in a 32-bit signed integer with a value of 1.
    --<
    --< @param value
    --< The value to find the least significant 1-bit for.
    --<
    --< @return
    --< The bit position of the least significant 1-bit. -1 is returned if there
    --< are no 1-bits.
    ----------------------------------------------------------------------------
    function Find_Lsb(value : in     Vkm_Int) return Vkm_Int;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation finds the least significant 1-bit for each component of a
    --< Vkm_GenIType vector.
    --<
    --< @description
    --< Applies Find_Lsb() component-wise on input Vkm_GenIType, returning a 
    --< Vkm_GenIType vector of the bit positions for the least significant 1-bit 
    --< in each component.
    ----------------------------------------------------------------------------
    function Find_Lsb is new GIT.Apply_Func_IV_RV(Find_Lsb);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation finds the least significant 1-bit in the 32-bit unsigned
    --< integer.
    --<
    --< @description
    --< Find the least significant bit in a 32-bit unsigned integer with a value of 1.
    --<
    --< @param value
    --< The value to find the least significant 1-bit for.
    --<
    --< @return
    --< The bit position of the least significant 1-bit. -1 is returned if there
    --< are no 1-bits.
    ----------------------------------------------------------------------------
    function Find_Lsb(value : in     Vkm_Uint) return Vkm_Int;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation finds the least significant 1-bit for each component of a
    --< Vkm_GenUType vector.
    --<
    --< @description
    --< Applies Find_Lsb() component-wise on input Vkm_GenUType, returning a 
    --< Vkm_GenIType vector of the bit positions for the least significant 1-bit 
    --< in each component.
    ----------------------------------------------------------------------------
    function Find_Lsb is new Apply_Func_IVU_RVI(Find_Lsb);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation finds the most significant 0-bit in the 32-bit signed
    --< integer.
    --<
    --< @description
    --< Find the most significant bit in a 32-bit signed integer with a value of 0.
    --<
    --< @param value
    --< The value to find the most significant 0-bit for.
    --<
    --< @return
    --< The bit position of the most significant 0-bit. -1 is returned if there
    --< are no 0-bits.
    ----------------------------------------------------------------------------
    function Find_Msb(value : in     Vkm_Int) return Vkm_Int;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation finds the most significant 0-bit for each component of a
    --< Vkm_GenIType vector.
    --<
    --< @description
    --< Applies Find_Msb() component-wise on input Vkm_GenIType, returning a 
    --< Vkm_GenIType vector of the bit positions for the most significant 0-bit 
    --< in each component.
    ----------------------------------------------------------------------------
    function Find_Msb is new GIT.Apply_Func_IV_RV(Find_Msb);
    
    
    ----------------------------------------------------------------------------
    --< @summary
    --< This operation finds the most significant 1-bit in the 32-bit unsigned
    --< integer.
    --<
    --< @description
    --< Find the most significant bit in a 32-bit unsigned integer with a value of 1.
    --<
    --< @param value
    --< The value to find the most significant 1-bit for.
    --<
    --< @return
    --< The bit position of the most significant 1-bit. -1 is returned if there
    --< are no 1-bits.
    ----------------------------------------------------------------------------
    function Find_Msb(value : in     Vkm_Uint) return Vkm_Int;


    ----------------------------------------------------------------------------
    --< @summary
    --< This operation finds the most significant 1-bit for each component of a
    --< Vkm_GenUType vector.
    --<
    --< @description
    --< Applies Find_Msb() component-wise on input Vkm_GenUType, returning a 
    --< Vkm_GenIType vector of the bit positions for the most significant 0-bit 
    --< in each component.
    ----------------------------------------------------------------------------
    function Find_Msb is new Apply_Func_IVU_RVI(Find_Msb);
    
    
    
end Vulkan.Math.Integers;
