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

package Vulkan.Math.Int is
    pragma Preelaborate;
    pragma Pure;
    
    -- An attempt to use an operation that is not defined for a type was made.
    UNDEFINED_OPERATION : exception;
    
    -- A conditional type taking on values of true or false.
    type Vkm_Int is new Interfaces.C.int
    with Default_Value => 0;
    
    -- Get the sign of the integer. +1 if positive, -1 if negative
    function Sign (x   : in     Vkm_Int) return Vkm_Int;
                           
    

end Vulkan.Math.Int;
