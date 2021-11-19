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
with Vulkan.Math.GenType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenType
--------------------------------------------------------------------------------
--< @summary
--< This package describes any length vector of Vkm_Bool type.
--<
--< @description
--< Provides an instantiation of the generic GenType  package with a Base_Type of
--< Vkm_Bool. This is used to provide the Vkm_GenBType subtype for the Vulkan Math
--< library.
--------------------------------------------------------------------------------
package Vulkan.Math.GenBType is
    pragma Preelaborate;
    pragma Pure;

    --< @private
    --< An instance of the generic GenType package, with Vkm_Bool as the Base_Type.
    package GBT is new Vulkan.Math.GenType(
        Base_Type => Vkm_Bool,
        Default_Value => false,
        Image => Vkm_Bool'Image,
        Unary_Minus => "-",
        Multiply => "*");

    --< A subtype of the instantiated Vkm_GenType that represents the GenBType
    --< described in the GLSL specification.
    subtype Vkm_GenBType is GBT.Vkm_GenType;


end Vulkan.Math.GenBType;
