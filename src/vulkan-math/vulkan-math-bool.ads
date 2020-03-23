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
--
-- This package provides a Boolean math type that can be used with Vulkan 
-- Shaders.
--
--------------------------------------------------------------------------------
with Interfaces.C;

package Vulkan.Math.Bool is
    pragma Preelaborate;
    pragma Pure;
    
    -- An attempt to use an operation that is not defined for a type was made.
    UNDEFINED_OPERATION : exception;
    
    -- A conditional type taking on values of true or false.
    type Vkm_Bool is new Boolean
    with Default_Value => False;
    for Vkm_Bool use (
        false => 0,
        true  => 1
    );
    for Vkm_Bool'Size use Interfaces.C.unsigned_char'Size;
    
    -- Scalar operations for the Vkm_Bool Type.
    -- Same as 'not'
    function "-"   (Right       : in     Vkm_Bool) return Vkm_Bool;
    
    -- Identity 
    function "+"   (Right       : in     Vkm_Bool) return Vkm_Bool;
    -- Same as Identity
    function "abs" (Right       : in     Vkm_Bool) return Vkm_Bool;
    
    -- Same as 'and'
    function "*"   (Left, Right : in     Vkm_Bool) return Vkm_Bool;
    -- Same as 'and'
    function "/"   (Left, Right : in     Vkm_Bool) return Vkm_Bool;
    
    -- Same as 'or'
    function "+"   (Left, Right : in     Vkm_Bool) return Vkm_Bool;
    
    -- Same as 'xor'
    function "-"   (Left, Right : in     Vkm_Bool) return Vkm_Bool;
    
    -- Invalid operations raise exceptions
    function "mod" (Left, Right : in     Vkm_Bool) return Vkm_Bool;
    function "rem" (Left, Right : in     Vkm_Bool) return Vkm_Bool;
    function "**"  (Left, Right : in     Vkm_Bool) return Vkm_Bool;
    
end Vulkan.Math.Bool;
