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
with Vulkan.Math.GenFType;

use Vulkan.Math.GenFType;

--------------------------------------------------------------------------------
--< @group Vulkan Math GenMatrix
--------------------------------------------------------------------------------
--< @summary
--< This provides an instantiation of a generic matrix for the Vkm_Float type.
--<
--< @description
--< The Vkm_Matrix type is a generic floating point matrix that can contain up to
--< 4 rows and 4 columns.
--------------------------------------------------------------------------------
package Vulkan.Math.GenFMatrix is
    pragma Preelaborate;
    pragma Pure;


    --< @private
    --< GFM is an instantiation of the GenMatrix package for the Vkm_Float type.
    package GFM is new Vulkan.Math.GenMatrix(
        Base_Type        => Vkm_Float,
        Base_Vector_Type => Vkm_GenFType,
        x                => GFT.x,
        y                => GFT.y,
        z                => GFT.z,
        w                => GFT.w,
        Make_GenType     => GFT.Make_GenType);
        
    --< The Vkm_GenFMatrix is a subtype of Vkm_GenMatrix from the instantiated GFM
    --< package.
    subtype Vkm_GenFMatrix is GFM.Vkm_GenMatrix;
end Vulkan.Math.GenFMatrix;

