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
-- This package instantiates Ada generic numerical operations for use by the 
-- Vulkan Math Library.
--------------------------------------------------------------------------------
with Ada.Numerics.Generic_Elementary_Functions;

private package Vulkan.Math.Numerics is
    pragma Preelaborate;
    pragma Pure;
    
    --Instantiation of Generic Elementary Functions for Float.
    package VKM_FLT_NEF is new 
        Ada.Numerics.Generic_Elementary_Functions(Float_Type => Vkm_Float);
    
    -- Instantiation of Generic Elemantry Functions for Double.
    package VKM_DBL_NEF is new
        Ada.Numerics.Generic_Elementary_Functions(Float_Type => Vkm_Double);
        
end Vulkan.Math.Numerics;
