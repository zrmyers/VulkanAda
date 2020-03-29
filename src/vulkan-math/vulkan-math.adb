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

package body Vulkan.Math is


    function To_Bool(value : in     Vkm_Uint  ) return Vkm_Bool is
    begin
        return Vkm_Bool(value /= 0);
    end To_Bool;

    function To_Bool(value : in     Vkm_Int   ) return Vkm_Bool is
    begin
        return Vkm_Bool(value /= 0);
    end To_Bool;

    function To_Bool(value : in     Vkm_Float) return Vkm_Bool is
    begin
        return Vkm_Bool(value /= 0.0);
    end To_Bool;


    function To_Bool(value : in     Vkm_Double) return Vkm_Bool is
    begin
        return Vkm_Bool(value /= 0.0);
    end To_Bool;


    ----------------------------------------------------------------------------


    function To_Uint (value : in     Vkm_Bool  ) return Vkm_Uint is
    begin
        return (if value then 1 else 0);
    end To_Uint;

    function To_Uint (value : in     Vkm_Int   ) return Vkm_Uint is

        function Convert_Int_To_Uint is
            new Ada.Unchecked_Conversion(Source => Vkm_Int, Target => Vkm_Uint);

    begin
        return Convert_Int_To_Uint(value);
    end To_Uint;


    function To_Uint (value : in     Vkm_Float ) return Vkm_Uint is
    begin
        return Vkm_Uint(Vkm_Float'Base(value));
    end To_Uint;

    function To_Uint (value : in     Vkm_Double) return Vkm_Uint is
    begin
        return Vkm_Uint(Vkm_Double'Base(value));
    end To_Uint;


    ----------------------------------------------------------------------------


    function To_Int (value : in     Vkm_Bool  ) return Vkm_Int is
    begin
        return (if value then 1 else 0);
    end To_Int;

    function To_Int (value : in     Vkm_Uint  ) return Vkm_Int is
        function Convert_Int_To_Uint is
            new Ada.Unchecked_Conversion(Source => Vkm_Uint, Target => Vkm_Int);
    begin
        return Convert_Int_To_Uint(value);
    end To_Int;

    function To_Int (value : in     Vkm_Float ) return Vkm_Int is
    begin
        return Vkm_Int(Vkm_Float'Base(value));
    end To_Int;

    function To_Int (value : in     Vkm_Double) return Vkm_Int is
    begin
        return Vkm_Int(Vkm_Double'Base(value));
    end To_Int;


    ----------------------------------------------------------------------------


    function To_Float (value : in     Vkm_Bool) return Vkm_Float is
    begin
        return (if value then 1.0 else 0.0);
    end To_Float;

    function To_Float (value : in     Vkm_Uint) return Vkm_Float is
    begin
        return Vkm_Float(Vkm_Uint'Base(value));
    end To_Float;

    function To_Float (value : in     Vkm_Int) return Vkm_Float is
    begin
        return Vkm_Float(Vkm_Int'Base(value));
    end To_Float;

    function To_Float (value : in     Vkm_Double) return Vkm_Float is
    begin
        return Vkm_Float(Vkm_Double'Base(value));
    end To_Float;


    ----------------------------------------------------------------------------


    function To_Double (value : in     Vkm_Bool ) return Vkm_Double is
    begin
        return (if value then 1.0 else 0.0);
    end To_Double;

    function To_Double (value : in     Vkm_Uint ) return Vkm_Double is
    begin
        return Vkm_Double(Vkm_Uint'Base(value));
    end To_Double;

    function To_Double (value : in     Vkm_Int  ) return Vkm_Double is
    begin
        return Vkm_Double(Vkm_Int'Base(value));
    end To_Double;

    function To_Double (value : in     Vkm_Float) return Vkm_Double is
    begin
        return Vkm_Double(Vkm_Float'Base(value));
    end To_Double;


end Vulkan.Math;
