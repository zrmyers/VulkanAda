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
-- This package proveds math types that can be used to interface with variables
-- that are input or output from shaders via Vulkan.
--------------------------------------------------------------------------------

package body Vulkan.Math.Bool is

    ----------------------------------------------------------------------------
    -- Definitions for Scalar Boolean Operations
    ----------------------------------------------------------------------------
    -- Same as 'not'
    function "-" (Right : Vkm_Bool) return Vkm_Bool is
    begin
        return not Right;
    end "-";

    -- Identity
    function "+" (Right : Vkm_Bool) return Vkm_Bool is
    begin
        return Right;
    end "+";

    -- Identity
    function "abs" (Right : Vkm_Bool) return Vkm_Bool is
    begin
        return Right;
    end "abs";
    
    -- Same as 'and'
    function "*" (Left, Right : Vkm_Bool) return Vkm_Bool is
    begin
        return Left and Right;
    end "*";

    function "/" (Left, Right : Vkm_Bool) return Vkm_Bool is
    begin
        return Left and Right;
    end "/";
    
    -- Same as 'or'
    function "+" (Left, Right : Vkm_Bool) return Vkm_Bool is
    begin
        return Left or Right;
    end "+";
    
    -- Same as 'xor'
    function "-" (Left, Right : Vkm_Bool) return Vkm_Bool is
    begin
        return Left xor Right;
    end "-";
    
    -- Invalid operations raise exceptions
    function "mod" (Left, Right : Vkm_Bool) return Vkm_Bool is
    begin
        raise UNDEFINED_OPERATION with "Operation mod is undefined for Vkm_Bool";
        return False;
    end "mod";
    
    
    function "rem" (Left, Right : Vkm_Bool) return Vkm_Bool is
    begin
        raise UNDEFINED_OPERATION with "Operation rem is undefined for Vkm_Bool";
        return False;
    end "rem";


    function "**" (Left, Right : Vkm_Bool) return Vkm_Bool is
    begin
        raise UNDEFINED_OPERATION with "Operation ""**"" is undefined for Vkm_Bool";
        return False;
    end "**";
    
end Vulkan.Math.Bool;
