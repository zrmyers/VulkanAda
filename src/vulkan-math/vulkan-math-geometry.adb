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
-- This package provides GLSL geometry operations.
--------------------------------------------------------------------------------
with Vulkan.Math.Exp;

use Vulkan.Math.Exp;

package body Vulkan.Math.Geometry is


    function Mag (x : in     Vkm_GenFType) return Vkm_Float is
        magnitude : Vkm_Float := 0.0;
    begin
        for Index in x.data'Range loop
            magnitude := magnitude + x.data(index) * x.data(index);
        end loop;
        return Sqrt(magnitude);
    end Mag;


    ----------------------------------------------------------------------------


    function Mag (x : in     Vkm_GenDType) return Vkm_Double is
        magnitude : Vkm_Double := 0.0;
    begin
        for index in x.data'Range loop
            magnitude := magnitude + x.data(index) * x.data(index);
        end loop;
        return Sqrt(magnitude);
    end Mag;


    ----------------------------------------------------------------------------


    function Dot (x, y : in     Vkm_GenFType) return Vkm_Float is
        dot_product : Vkm_Float := 0.0;
    begin
        for index in x.data'Range loop
            dot_product := dot_product + x.data(index) * y.data(index);
        end loop;
        return dot_product;
    end Dot;


    ----------------------------------------------------------------------------


    function Dot (x, y : in     Vkm_GenDType) return Vkm_Double is
        dot_product : Vkm_Double := 0.0;
    begin
        for index in x.data'Range loop
            dot_product := dot_product + x.data(index) * y.data(index);
        end loop;
        return dot_product;
    end Dot;


    ----------------------------------------------------------------------------


    function Cross (x, y : in     Vkm_Vec3 ) return Vkm_Vec3 is
        cross_product : Vkm_Vec3 := Make_Vec3(0.0);
    begin
    
        cross_product.x(x.y*y.z - x.z * y.y)
                     .y(x.z*y.x - x.x * y.z)
                     .z(x.x*y.y - x.y * y.x);
        return cross_product;
    end Cross;


    ----------------------------------------------------------------------------


    function Cross (x, y : in     Vkm_Dvec3) return Vkm_Dvec3 is
        cross_product : Vkm_Dvec3 := Make_Dvec3(0.0);
    begin
        cross_product.x(x.y*y.z - x.z * y.y)
                     .y(x.z*y.x - x.x * y.z)
                     .z(x.x*y.y - x.y * y.x);
        return cross_product;
    end Cross;


    ----------------------------------------------------------------------------


    function Refract(i, n : in     Vkm_GenFType;
                     eta  : in     Vkm_Float   ) return Vkm_GenFType is
        dot_n_i : constant Vkm_Float := Dot(n, i);
        k : constant Vkm_Float := 1.0 - eta * eta * (1.0 - dot_n_i * dot_n_i);
    begin
        return (if k < 0.0 then GFT.Make(Last_Index => i.Last_Index, value => 0.0)
                           else eta*i - (eta * dot_n_i + Sqrt(k)) * N);
    end Refract;

    function Refract(i, n : in     Vkm_GenDType;
                     eta  : in     Vkm_Double  ) return Vkm_GenDType is
        dot_n_i : constant Vkm_Double := Dot(n, i);
        k : constant Vkm_Double := 1.0 - eta * eta * (1.0 - dot_n_i * dot_n_i);
    begin
        return (if k < 0.0 then GDT.Make(Last_Index => i.Last_Index, value => 0.0)
                           else eta*i - (eta * dot_n_i + Sqrt(k)) * N);
    end Refract;

end Vulkan.Math.Geometry;
