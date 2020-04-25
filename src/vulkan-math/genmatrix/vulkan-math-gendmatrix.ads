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
with Vulkan.Math.GenMatrix;
with Vulkan.Math.GenDType;

use Vulkan.Math.GenDType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenMatrix
--------------------------------------------------------------------------------
--< @summary
--< This package provides an instantiation of a matrix for the Vkm_Double type.
--<
--< @description
--< The Vkm_GenDMatrix type is a matrix of double precision floating point numbers
--< that can have up to 4 rows and 4 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.GenDMatrix is
    pragma Preelaborate;
    pragma Pure;
    
    --< @private
    --< GDM is an instantiation of the GenMatrix package for the Vkm_Double type.
    package GDM is new Vulkan.Math.GenMatrix(
        Base_Type        => Vkm_Double,
        Base_Vector_Type => Vkm_GenDType,
        x                => GDT.x,
        y                => GDT.y,
        z                => GDT.z,
        w                => GDT.w,
        Make_GenType     => GDT.Make_GenType);
        
    --< The Vkm_GenDMatrix is a subtype of Vkm_GenMatrix for the instantiated GDM
    --< package.
    subtype Vkm_GenDMatrix is GDM.Vkm_GenMatrix;
    
    
end Vulkan.Math.GenDMatrix;
