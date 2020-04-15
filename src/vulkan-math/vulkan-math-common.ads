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

with Vulkan.Math.GenFType;
with Vulkan.Math.GenDType;
with Vulkan.Math.GenIType;
with Vulkan.Math.GenUType;
with Vulkan.Math.GenBType;
with Vulkan.Math.Numerics;
with ada.Unchecked_Conversion;

-- Uses
use Vulkan.Math.GenFType;
use Vulkan.Math.GenDType;
use Vulkan.Math.GenIType;
use Vulkan.Math.GenUType;
use Vulkan.Math.GenBType;

--------------------------------------------------------------------------------
--< @group Vulkan Math Functions
--------------------------------------------------------------------------------
--< @summary
--< This package provides GLSL Common Built-in functions.
--<
--< @description
--< All common functions operate component-wise.
--------------------------------------------------------------------------------
package Vulkan.Math.Common is
    pragma Preelaborate;
    pragma Pure;

    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Computes the absolute value of scalar Vkm_Float x.
    --<
    --< @param x 
    --< The input parameter
    --<
    --< @return 
    --< Returns x if x >= 0.0; otherwise it returns -x.
    ----------------------------------------------------------------------------
    function Absolute_Value (x : in     Vkm_Float ) return Vkm_Float is
        (if x >= 0.0 then x else -x) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Computes the absolute value of scalar Vkm_Double x.
    --<
    --< @param x 
    --< The input parameter
    --<
    --< @return 
    --< Returns x if x >= 0.0; otherwise it returns -x.
    ----------------------------------------------------------------------------
    function Absolute_Value (x : in     Vkm_Double) return Vkm_Double is
        (if x >= 0.0 then x else -x) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Computes the absolute value of scalar Vkm_Int x.
    --<
    --< @param x 
    --< The input parameter
    --<
    --< @return 
    --< Returns x if x >= 0; otherwise it returns -x.
    ----------------------------------------------------------------------------
    function Absolute_Value (x : in     Vkm_Int) return Vkm_Int is
        (if x >= 0 then x else -x)   with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Applies Absolute_Value() component-wise on a GenFType vector, returning
    --< a GenFType vector with the result.
    ----------------------------------------------------------------------------
    function Absolute_Value is new GFT.Apply_Func_IV_RV(Absolute_Value);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Applies Absolute_Value() component-wise on a GenDType vector, returning
    --< a GenDType vector with the result.
    ----------------------------------------------------------------------------
    function Absolute_Value is new GDT.Apply_Func_IV_RV(Absolute_Value);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the absolute value of x.
    --<
    --< @description
    --< Applies Absolute_Value() component-wise on a GenIType vector, returning
    --< a GenIType vector with the result.
    ----------------------------------------------------------------------------
    function Absolute_Value is new GIT.Apply_Func_IV_RV(Absolute_Value);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of a scalar Vkm_Float.
    --<
    --< @param x 
    --< The scalar input parameter
    --<
    --< @return 
    --< Returns one of the following:
    --< -  1 if X > 0
    --< -  0 if X = 0
    --< - -1 if x < 0
    ----------------------------------------------------------------------------
    function Sign (x : in     Vkm_Float ) return Vkm_Float is
        (if x > 0.0 then 1.0 elsif x < 0.0 then -1.0 else 0.0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of a scalar Vkm_Double.
    --<
    --< @param x 
    --< The scalar input parameter
    --<
    --< @return 
    --< Returns one of the following:
    --< -  1 if X > 0
    --< -  0 if X = 0
    --< - -1 if x < 0
    ----------------------------------------------------------------------------
    function Sign (x : in     Vkm_Double) return Vkm_Double is
        (if x > 0.0 then 1.0 elsif x < 0.0 then -1.0 else 0.0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of a scalar Vkm_Int.
    --<
    --< @param x 
    --< The scalar input parameter
    --<
    --< @return 
    --< Returns one of the following:
    --< -  1 if X > 0
    --< -  0 if X = 0
    --< - -1 if x < 0
    ----------------------------------------------------------------------------
    function Sign (x : in     Vkm_Int   ) return Vkm_Int is
        (if x > 0 then 1 elsif x < 0 then -1 else 0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of each component of GenFType vector x.
    ----------------------------------------------------------------------------
    function Sign is new GFT.Apply_Func_IV_RV(Sign);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of each component of GenDType vector x.
    ----------------------------------------------------------------------------
    function Sign is new GDT.Apply_Func_IV_RV(Sign);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determines the sign of x.
    --<
    --< @description
    --< Determines the sign of each component of GenDType vector x.
    ----------------------------------------------------------------------------
    function Sign is new GIT.Apply_Func_IV_RV(Sign);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the floor of x.
    --<
    --< @description
    --< Computes the floor, y, as the nearest integer that is less than or equal
    --< to scalar Vkm_Float x.
    --<
    --< @param x 
    --< The value for which the floor is computed.
    --<
    --< @return 
    --< Returns the floor, y.
    ----------------------------------------------------------------------------
    function Floor (x : in     Vkm_Float)  return Vkm_Float renames Vkm_Float'Floor;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the floor of x.
    --<
    --< @description
    --< Computes the floor, y, as the nearest integer that is less than or equal
    --< to scalar Vkm_Double x.
    --<
    --< @param x 
    --< The value for which the floor is computed.
    --<
    --< @return 
    --< Returns the floor, y.
    ----------------------------------------------------------------------------
    function Floor (x : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Floor;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the floor of x.
    --<
    --< @description
    --< Computes the floor for each component of the GenFType, returning a vector
    --< containing the component-wise result.
    ----------------------------------------------------------------------------
    function Floor is new GFT.Apply_Func_IV_RV(Floor);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the floor of x.
    --<
    --< @description
    --< Computes the floor for each component of the GenDType, returning a vector
    --< containing the component-wise result.
    ----------------------------------------------------------------------------
    function Floor is new GDT.Apply_Func_IV_RV(Floor);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the truncation of x.
    --<
    --< @description
    --< Computes the trunction of x, y, as the nearest integer to x whose absolute
    --< value is less than or equal to the absolute value of x.
    --<
    --< @param x
    --< The value on which truncation is performed.
    --<
    --< @return 
    --< The truncation of x, y.
    ----------------------------------------------------------------------------
    function Trunc (x : in     Vkm_Float) return Vkm_Float renames Vkm_Float'Truncation;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the truncation of x.
    --<
    --< @description
    --< Computes the trunction of x, y, as the nearest integer to x whose absolute
    --< value is less than or equal to the absolute value of x.
    --<
    --< @param x
    --< The value on which truncation is performed.
    --<
    --< @return 
    --< The truncation of x, y.
    ----------------------------------------------------------------------------
    function Trunc (x : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Truncation;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the truncation of x.
    --<
    --< @description
    --< Computes component-wise trunction on a vector x, returning a vector
    --< with the result.
    ----------------------------------------------------------------------------
    function Trunc is new GFT.Apply_Func_IV_RV(Trunc);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the truncation of x.
    --<
    --< @description
    --< Computes component-wise trunction on a vector x, returning a vector
    --< with the result.
    ----------------------------------------------------------------------------
    function Trunc is new GDT.Apply_Func_IV_RV(Trunc);


    ----------------------------------------------------------------------------
    --< @summary
    --< Rounds x to the nearest integer. 
    --< 
    --< @description
    --< Rounds the value x to the nearest integer, rounding away from 0 if the
    --< fraction part of x is equal to 0.5.
    --<
    --< @param x 
    --< The input parameter.
    --<
    --< @return 
    --< The rounded integer.
    ----------------------------------------------------------------------------
    function Round (x : in     Vkm_Float   ) return Vkm_Float renames Vkm_Float'Rounding;


    ----------------------------------------------------------------------------
    --< @summary
    --< Rounds x to the nearest integer. 
    --< 
    --< @description
    --< Rounds the value x to the nearest integer, rounding away from 0 if the
    --< fraction part of x is equal to 0.5.
    --<
    --< @param x 
    --< The input parameter.
    --<
    --< @return 
    --< The rounded integer.
    ----------------------------------------------------------------------------
    function Round (x : in     Vkm_Double  ) return Vkm_Double renames Vkm_Double'Rounding;


    ----------------------------------------------------------------------------
    --< @summary
    --< Rounds x to the nearest integer. 
    --< 
    --< @description
    --< Apply the Round() function to each compoent of input vector x, returning
    --< a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function Round is new GFT.Apply_Func_IV_RV(Round);


    ----------------------------------------------------------------------------
    --< @summary
    --< Rounds x to the nearest integer. 
    --< 
    --< @description
    --< Apply the Round() function to each compoent of input vector x, returning
    --< a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function Round is new GDT.Apply_Func_IV_RV(Round);


    ----------------------------------------------------------------------------
    --< @summary
    --< Rounds x to the nearest integer. 
    --<
    --< @description
    --< Rounds x to the nearest integer, rounding to the nearest even integer if
    --< the fraction part of x is equal to 0.5.
    --<
    --< @param x 
    --< The input parameter.
    --<
    --< @return 
    --<The rounded integer.
    ----------------------------------------------------------------------------
    function RoundEven (x : in     Vkm_Float) return Vkm_Float renames Vkm_Float'Unbiased_Rounding;


    ----------------------------------------------------------------------------
    --< @summary
    --< Rounds x to the nearest integer. 
    --<
    --< @description
    --< Rounds x to the nearest integer, rounding to the nearest even integer if
    --< the fraction part of x is equal to 0.5.
    --<
    --< @param x 
    --< The input parameter.
    --<
    --< @return 
    --<The rounded integer.
    ----------------------------------------------------------------------------
    function RoundEven (x : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Unbiased_Rounding;


    ----------------------------------------------------------------------------
    --< @summary
    --< Rounds x to the nearest integer. 
    --< 
    --< @description
    --< Apply the Round_Even() function to each compoent of input vector x, returning
    --< a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function RoundEven is new GFT.Apply_Func_IV_RV(RoundEven);


    ----------------------------------------------------------------------------
    --< @summary
    --< Rounds x to the nearest integer. 
    --< 
    --< @description
    --< Apply the Round_Even() function to each compoent of input vector x, returning
    --< a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function RoundEven is new GdT.Apply_Func_IV_RV(RoundEven);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the ceil of x.
    --<
    --< @description
    --< Determines the nearest integer greater than or equal to x.
    --<
    --< @param x 
    --< The input parameter.
    --<
    --< @return 
    --< The ceiling of x.
    ----------------------------------------------------------------------------
    function Ceil (x : in     Vkm_Float   ) return Vkm_Float renames Vkm_Float'Ceiling;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the ceil of x.
    --<
    --< @description
    --< Determines the nearest integer greater than or equal to x.
    --<
    --< @param x 
    --< The input parameter.
    --<
    --< @return 
    --< The ceiling of x.
    ----------------------------------------------------------------------------
    function Ceil (x : in     Vkm_Double  ) return Vkm_Double renames Vkm_Double'Ceiling;


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the ceil of x.
    --< 
    --< @description
    --< Apply the Ceil() function to each compoent of input vector x, returning
    --< a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function Ceil is new GFT.Apply_Func_IV_RV(Ceil);


    ----------------------------------------------------------------------------
    --< @summary
    --< Computes the ceil of x.
    --< 
    --< @description
    --< Apply the Ceil() function to each compoent of input vector x, returning
    --< a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function Ceil is new GDT.Apply_Func_IV_RV(Ceil);


    ----------------------------------------------------------------------------
    --< @summary
    --< Get the fraction part of x.
    --<
    --< @description
    --< Get the fraction part of x by subtracting the floor of x:
    --<
    --<     fraction := x - floor(x);
    --<
    --< @param x 
    --< The input parameter.
    --<
    --< @return 
    --< The fraction part of x.
    ----------------------------------------------------------------------------
    function Fract (x : in     Vkm_Float   ) return Vkm_Float is
        (x - Floor(x)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Get the fraction part of x.
    --<
    --< @description
    --< Get the fraction part of x by subtracting the floor of x:
    --<
    --<     fraction := x - floor(x);
    --<
    --< @param x 
    --< The input parameter.
    --<
    --< @return 
    --< The fraction part of x.
    ----------------------------------------------------------------------------
    function Fract (x : in     Vkm_Double  ) return Vkm_Double is
        (x - Floor(x)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Get the fraction part of x.
    --< 
    --< @description
    --< Apply the Fract() function to each compoent of input vector x, returning
    --< a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function Fract is new GFT.Apply_Func_IV_RV(Fract);


    ----------------------------------------------------------------------------
    --< @summary
    --< Get the fraction part of x.
    --< 
    --< @description
    --< Apply the Fract() function to each compoent of input vector x, returning
    --< a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function Fract is new GDT.Apply_Func_IV_RV(Fract);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the modulo of x and y.
    --<
    --< @description
    --< Compute the modulo of x and y:
    --<
    --<     x mod y = x - y * floor(x / y)
    --<
    --< @param x
    --< The value to which the modulus is applied.
    --<
    --< @param y
    --< The modulus.
    --<
    --< @return 
    --< The modulus of x in y.
    ----------------------------------------------------------------------------
    function Modulo (x, y : in     Vkm_Float) return Vkm_Float is
        (x - y * Floor(x / y)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the modulo of x in y.
    --<
    --< @description
    --< Compute the modulo of x in y:
    --<
    --<     x mod y = x - y * floor(x / y)
    --<
    --< @param x
    --< The value to which the modulus is applied.
    --<
    --< @param y
    --< The modulus.
    --<
    --< @return 
    --< The modulus of x in y.
    ----------------------------------------------------------------------------
    function Modulo (x, y : in     Vkm_Double) return Vkm_Double is
        (x - y * Floor(x / y)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the modulo of x in y.
    --< 
    --< @description
    --< Apply the Modulo() function to each compoent of input vectors x and y, 
    --< returning a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function Modulo is new GFT.Apply_Func_IV_IV_RV(Modulo);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the modulo of x in y.
    --< 
    --< @description
    --< Apply the Modulo() function to each compoent of input vectors x and y, 
    --< returning a vector with the component-wise result.
    ----------------------------------------------------------------------------
    function Modulo is new GDT.Apply_Func_IV_IV_RV(Modulo);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the modf of x.
    --<
    --< @description
    --< Compute the modf of x, seperating the value into its integer and fraction
    --< parts. The integer part is an output parameter and the fraction
    --< part is the return value for the function.
    ----------------------------------------------------------------------------
    function Modf is new Vulkan.Math.Numerics.Compute_Modf(Vkm_Float);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the modf of x.
    --<
    --< @description
    --< Compute the modf of x, seperating the value into its integer and fraction
    --< parts. The integer part is an output parameter and the fraction
    --< part is the return value for the function.
    ----------------------------------------------------------------------------
    function Modf is new Vulkan.Math.Numerics.Compute_Modf(Vkm_Double);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the modf of x.
    --< 
    --< @description
    --< Apply the Modulo() function to each compoent of input vector x, setting
    --< an output vector parameter to the integer parts of components, and returning
    --< the fraction parts.
    ----------------------------------------------------------------------------
    function Modf is new GFT.Apply_Func_IV_OV_RV(Modf);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the modf of x.
    --< 
    --< @description
    --< Apply the Modulo() function to each compoent of input vector x, setting
    --< an output vector parameter to the integer parts of components, and returning
    --< the fraction parts.
    ----------------------------------------------------------------------------
    function Modf is new GDT.Apply_Func_IV_OV_RV(Modf);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the min between the two values x and y.
    --<
    --< @description
    --< Compute the min of x and y, which is the smallest of the two numbers.
    --<
    --< @param x 
    --< The input parameter 'x'.
    --<
    --< @param y 
    --< The input parameter 'y'.
    --<
    --< @return 
    --< The minimum of x and y.
    ----------------------------------------------------------------------------
    function Min (x, y : in     Vkm_Float ) return Vkm_Float  renames Vkm_Float'Min;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the min between the two values x and y.
    --<
    --< @description
    --< Compute the min of x and y, which is the smallest of the two numbers.
    --<
    --< @param x 
    --< The input parameter 'x'.
    --<
    --< @param y 
    --< The input parameter 'y'.
    --<
    --< @return 
    --< The minimum of x and y.
    ----------------------------------------------------------------------------
    function Min (x, y : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Min;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the min between the two values x and y.
    --<
    --< @description
    --< Compute the min of x and y, which is the smallest of the two numbers.
    --<
    --< @param x 
    --< The input parameter 'x'.
    --<
    --< @param y 
    --< The input parameter 'y'.
    --<
    --< @return 
    --< The minimum of x and y.
    ----------------------------------------------------------------------------
    function Min (x, y : in     Vkm_Uint  ) return Vkm_Uint   renames Vkm_Uint'Min;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the min between the two values x and y.
    --<
    --< @description
    --< Compute the min of x and y, which is the smallest of the two numbers.
    --<
    --< @param x 
    --< The input parameter 'x'.
    --<
    --< @param y 
    --< The input parameter 'y'.
    --<
    --< @return 
    --< The minimum of x and y.
    ----------------------------------------------------------------------------
    function Min (x, y : in     Vkm_Int   ) return Vkm_Int    renames Vkm_Int'Min;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the min between the two values x and y.
    --<
    --< @description
    --< Apply the Min() function component-wise on the two input vectors, returning
    --< the resulting vector.
    ----------------------------------------------------------------------------
    function Min is new GFT.Apply_Func_IV_IV_RV(Min);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the min between the two values x and y.
    --<
    --< @description
    --< Apply the Min() function component-wise on the two input vectors, returning
    --< the resulting vector.
    ----------------------------------------------------------------------------
    function Min is new GDT.Apply_Func_IV_IV_RV(Min);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the min between the two values x and y.
    --<
    --< @description
    --< Apply the Min() function component-wise on the two input vectors, returning
    --< the resulting vector.
    ----------------------------------------------------------------------------
    function Min is new GUT.Apply_Func_IV_IV_RV(Min);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the min between the two values x and y.
    --<
    --< @description
    --< Apply the Min() function component-wise on the two input vectors, returning
    --< the resulting vector.
    ----------------------------------------------------------------------------
    function Min is new GIT.Apply_Func_IV_IV_RV(Min);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the max between the two values x and y.
    --<
    --< @description
    --< Compute the max of x and y, which is the greatest of the two numbers.
    --<
    --< @param x 
    --< The input parameter 'x'.
    --<
    --< @param y 
    --< The input parameter 'y'.
    --<
    --< @return 
    --< The maximum of x and y.
    ----------------------------------------------------------------------------
    function Max (x, y : in     Vkm_Float ) return Vkm_Float  renames Vkm_Float'Max;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the max between the two values x and y.
    --<
    --< @description
    --< Compute the max of x and y, which is the greatest of the two numbers.
    --<
    --< @param x 
    --< The input parameter 'x'.
    --<
    --< @param y 
    --< The input parameter 'y'.
    --<
    --< @return 
    --< The maximum of x and y.
    ----------------------------------------------------------------------------
    function Max (x, y : in     Vkm_Double) return Vkm_Double renames Vkm_Double'Max;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the max between the two values x and y.
    --<
    --< @description
    --< Compute the max of x and y, which is the greatest of the two numbers.
    --<
    --< @param x 
    --< The input parameter 'x'.
    --<
    --< @param y 
    --< The input parameter 'y'.
    --<
    --< @return 
    --< The maximum of x and y.
    ----------------------------------------------------------------------------
    function Max (x, y : in     Vkm_Uint  ) return Vkm_Uint   renames Vkm_Uint'Max;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the max between the two values x and y.
    --<
    --< @description
    --< Compute the max of x and y, which is the greatest of the two numbers.
    --<
    --< @param x 
    --< The input parameter 'x'.
    --<
    --< @param y 
    --< The input parameter 'y'.
    --<
    --< @return 
    --< The maximum of x and y.
    ----------------------------------------------------------------------------
    function Max (x, y : in     Vkm_Int   ) return Vkm_Int    renames Vkm_Int'Max;


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the max between the two values x and y.
    --<
    --< @description
    --< Apply the Max() function component-wise on the two input vectors, returning
    --< the resulting vector.
    ----------------------------------------------------------------------------
    function Max is new GFT.Apply_Func_IV_IV_RV(Max);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the max between the two values x and y.
    --<
    --< @description
    --< Apply the Max() function component-wise on the two input vectors, returning
    --< the resulting vector.
    ----------------------------------------------------------------------------
    function Max is new GDT.Apply_Func_IV_IV_RV(Max);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the max between the two values x and y.
    --<
    --< @description
    --< Apply the Max() function component-wise on the two input vectors, returning
    --< the resulting vector.
    ----------------------------------------------------------------------------
    function Max is new GUT.Apply_Func_IV_IV_RV(Max);


    ----------------------------------------------------------------------------
    --< @summary
    --< Compute the max between the two values x and y.
    --<
    --< @description
    --< Apply the Max() function component-wise on the two input vectors, returning
    --< the resulting vector.
    ----------------------------------------------------------------------------
    function Max is new GIT.Apply_Func_IV_IV_RV(Max);


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Clamp x between minVal and maxVal using the following algorithm:
    --<
    --<     clamp := min( max( x , minVal), maxVal);
    --<
    --< Results are undefined for minVal > maxVal.
    --<
    --< @param x
    --< The input parameter 'x'.
    --<
    --< @param minVal 
    --< The minimum value in range.
    --<
    --< @param maxVal 
    --< The maximum value in range.
    --<
    --< @return Returns:
    --< The value x clamped between minVal and maxVal.
    ----------------------------------------------------------------------------
    function Clamp (x, minVal, maxVal : in     Vkm_Float)  return Vkm_Float is
        (Min(Max(x,minVal),maxVal)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Clamp x between minVal and maxVal using the following algorithm:
    --<
    --<     clamp := min( max( x , minVal), maxVal);
    --<
    --< Results are undefined for minVal > maxVal.
    --<
    --< @param x
    --< The input parameter 'x'.
    --<
    --< @param minVal 
    --< The minimum value in range.
    --<
    --< @param maxVal 
    --< The maximum value in range.
    --<
    --< @return Returns:
    --< The value x clamped between minVal and maxVal.
    ----------------------------------------------------------------------------
    function Clamp (x, minVal, maxVal : in     Vkm_Double) return Vkm_Double is
        (Min(Max(x,minVal),maxVal)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Clamp x between minVal and maxVal using the following algorithm:
    --<
    --<     clamp := min( max( x , minVal), maxVal);
    --<
    --< Results are undefined for minVal > maxVal.
    --<
    --< @param x
    --< The input parameter 'x'.
    --<
    --< @param minVal 
    --< The minimum value in range.
    --<
    --< @param maxVal 
    --< The maximum value in range.
    --<
    --< @return Returns:
    --< The value x clamped between minVal and maxVal.
    ----------------------------------------------------------------------------
    function Clamp (x, minVal, maxVal : in     Vkm_Uint)   return Vkm_Uint is
        (Min(Max(x,minVal),maxVal)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Clamp x between minVal and maxVal using the following algorithm:
    --<
    --<     clamp := min( max( x , minVal), maxVal);
    --<
    --< Results are undefined for minVal > maxVal.
    --<
    --< @param x
    --< The input parameter 'x'.
    --<
    --< @param minVal 
    --< The minimum value in range.
    --<
    --< @param maxVal 
    --< The maximum value in range.
    --<
    --< @return Returns:
    --< The value x clamped between minVal and maxVal.
    ----------------------------------------------------------------------------
    function Clamp (x, minVal, maxVal : in     Vkm_Int)    return Vkm_Int is
        (Min(Max(x,minVal),maxVal)) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Apply the Clamp() function component-wise on the three input vectors, returning
    --< a vector with the result.
    ----------------------------------------------------------------------------
    function Clamp is new GFT.Apply_Func_IV_IV_IV_RV(Clamp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Apply the Clamp() function component-wise on the three input vectors, returning
    --< a vector with the result.
    ----------------------------------------------------------------------------
    function Clamp is new GDT.Apply_Func_IV_IV_IV_RV(Clamp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Apply the Clamp() function component-wise on the three input vectors, returning
    --< a vector with the result.
    ----------------------------------------------------------------------------
    function Clamp is new GUT.Apply_Func_IV_IV_IV_RV(Clamp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Apply the Clamp() function component-wise on the three input vectors, returning
    --< a vector with the result.
    ----------------------------------------------------------------------------
    function Clamp is new GIT.Apply_Func_IV_IV_IV_RV(Clamp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Apply the Clamp() function component-wise on one input vectors using two
    --< input scalars for the minVal and maxVal of each component, returning a
    --< vector with the result.
    ----------------------------------------------------------------------------
    function Clamp is new GFT.Apply_Func_IV_IS_IS_RV(Clamp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Clamp x between minVal and maxVal.
    --<
    --< @description
    --< Apply the Clamp() function component-wise on one input vectors using two
    --< input scalars for the minVal and maxVal of each component, returning a
    --< vector with the result.
    ----------------------------------------------------------------------------
    function Clamp is new GDT.Apply_Func_IV_IS_IS_RV(Clamp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix x and y together using a linear blend function.
    --<
    --< @description
    --< Mix the values 'x' and 'y' together using a linear blend function:
    --<
    --<     blend := 'x * (1 - a) + y * a'
    --<
    --< @param x 
    --< The input parameter 'x' that is mixed with 'y'
    --<
    --< @param y 
    --< The input paramter 'y' that is mixed with 'x'
    --<
    --< @param a 
    --< The input parameter 'a' which is a coefficient in the linear blend function.
    --<
    --< @return X mixed with y.
    ----------------------------------------------------------------------------
    function Mix (x, y, a : in     Vkm_Float) return Vkm_Float is
        (x * (1.0 - a) + y * a) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix x and y together using a linear blend function.
    --<
    --< @description
    --< Mix the values 'x' and 'y' together using a linear blend function:
    --<
    --<     blend := 'x * (1 - a) + y * a'
    --<
    --< @param x 
    --< The input parameter 'x' that is mixed with 'y'
    --<
    --< @param y 
    --< The input paramter 'y' that is mixed with 'x'
    --<
    --< @param a 
    --< The input parameter 'a' which is a coefficient in the linear blend function.
    --<
    --< @return X mixed with y.
    ----------------------------------------------------------------------------
    function Mix (x, y, a : in     Vkm_Double) return Vkm_Double is
        (x * (1.0 - a) + y * a) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix x and y together using a linear blend function.
    --<
    --< @description
    --< Apply the linear mix function component-wise on input vectors x and y, using
    --< components from input vector a as the blend coefficient. The resulting vector
    --< is returned.
    ----------------------------------------------------------------------------
    function Mix is new GFT.Apply_Func_IV_IV_IV_RV(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix x and y together using a linear blend function.
    --<
    --< @description
    --< Apply the linear mix function component-wise on input vectors x and y, using
    --< input scalar a as the blend coefficient. The resulting vector is returned.
    ----------------------------------------------------------------------------
    function Mix is new GFT.Apply_Func_IV_IV_IS_RV(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix x and y together using a linear blend function.
    --<
    --< @description
    --< Apply the linear mix function component-wise on input vectors x and y, using
    --< components from input vector a as the blend coefficient. The resulting vector
    --< is returned.
    ----------------------------------------------------------------------------
    function Mix is new GDT.Apply_Func_IV_IV_IV_RV(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix x and y together using a linear blend function.
    --<
    --< @description
    --< Apply the linear mix function component-wise on input vectors x and y, using
    --< input scalar a as the blend coefficient. The resulting vector is returned.
    ----------------------------------------------------------------------------
    function Mix is new GDT.Apply_Func_IV_IV_IS_RV(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Mix the values 'x' and 'y' together using a boolean blend function:
    --< - x if a is true
    --< - y if a is false
    --<
    --< @param x 
    --< The input parameter 'x' that is mixed with 'y'
    --<
    --< @param y 
    --< The input parameter 'y' that is mixed with 'x'
    --<
    --< @param a 
    --< The input parameter 'a' which is the boolean mixing coefficient.
    --<
    --< @return 
    --< The mixture of x with y.
    ----------------------------------------------------------------------------
    function Mix (x, y : in     Vkm_Float;
                  a    : in     Vkm_Bool) return Vkm_Float is
        (if a then x else y ) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Mix the values 'x' and 'y' together using a boolean blend function:
    --< - x if a is true
    --< - y if a is false
    --<
    --< @param x 
    --< The input parameter 'x' that is mixed with 'y'
    --<
    --< @param y 
    --< The input parameter 'y' that is mixed with 'x'
    --<
    --< @param a 
    --< The input parameter 'a' which is the boolean mixing coefficient.
    --<
    --< @return 
    --< The mixture of x with y.
    ----------------------------------------------------------------------------
    function Mix (x, y : in     Vkm_Double;
                  a    : in     Vkm_Bool) return Vkm_Double is
        (if a then x else y ) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Mix the values 'x' and 'y' together using a boolean blend function:
    --< - x if a is true
    --< - y if a is false
    --<
    --< @param x 
    --< The input parameter 'x' that is mixed with 'y'
    --<
    --< @param y 
    --< The input parameter 'y' that is mixed with 'x'
    --<
    --< @param a 
    --< The input parameter 'a' which is the boolean mixing coefficient.
    --<
    --< @return 
    --< The mixture of x with y.
    ----------------------------------------------------------------------------
    function Mix (x, y : in     Vkm_Uint;
                  a    : in     Vkm_Bool) return Vkm_Uint is
        (if a then x else y ) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Mix the values 'x' and 'y' together using a boolean blend function:
    --< - x if a is true
    --< - y if a is false
    --<
    --< @param x 
    --< The input parameter 'x' that is mixed with 'y'
    --<
    --< @param y 
    --< The input parameter 'y' that is mixed with 'x'
    --<
    --< @param a 
    --< The input parameter 'a' which is the boolean mixing coefficient.
    --<
    --< @return 
    --< The mixture of x with y.
    ----------------------------------------------------------------------------
    function Mix (x, y : in     Vkm_Int;
                  a    : in     Vkm_Bool) return Vkm_Int is
        (if a then x else y ) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Mix the values 'x' and 'y' together using a boolean blend function:
    --< - x if a is true
    --< - y if a is false
    --<
    --< @param x 
    --< The input parameter 'x' that is mixed with 'y'
    --<
    --< @param y 
    --< The input parameter 'y' that is mixed with 'x'
    --<
    --< @param a 
    --< The input parameter 'a' which is the boolean mixing coefficient.
    --<
    --< @return 
    --< The mixture of x with y.
    ----------------------------------------------------------------------------
    function Mix (x, y : in     Vkm_Bool;
                  a    : in     Vkm_Bool) return Vkm_Bool is
        (if a then x else y ) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Applies the boolean Mix() function component wise to input vectors x, y 
    --< and a, where components of a are used as the mixing coefficent. The resulting
    --< vector is returned.
    ----------------------------------------------------------------------------
    function Mix is new Apply_Func_IVF_IVF_IVB_RVF(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Applies the boolean Mix() function component wise to input vectors x, y 
    --< and a, where components of a are used as the mixing coefficent. The resulting
    --< vector is returned.
    ----------------------------------------------------------------------------
    function Mix is new Apply_Func_IVD_IVD_IVB_RVD(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Applies the boolean Mix() function component wise to input vectors x, y 
    --< and a, where components of a are used as the mixing coefficent. The resulting
    --< vector is returned.
    ----------------------------------------------------------------------------
    function Mix is new Apply_Func_IVI_IVI_IVB_RVI(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Applies the boolean Mix() function component wise to input vectors x, y 
    --< and a, where components of a are used as the mixing coefficent. The resulting
    --< vector is returned.
    ----------------------------------------------------------------------------
    function Mix is new Apply_Func_IVU_IVU_IVB_RVU(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Mix the values 'x' and 'y' together using a boolean blend function.
    --<
    --< @description
    --< Applies the boolean Mix() function component wise to input vectors x, y 
    --< and a, where components of a are used as the mixing coefficent. The resulting
    --< vector is returned.
    ----------------------------------------------------------------------------
    function Mix is new GBT.Apply_Func_IV_IV_IV_RV(Mix);


    ----------------------------------------------------------------------------
    --< @summary
    --< Unit step function.
    --<
    --< @description
    --< Compute the step function as follows:
    --< - Return 0.0 if x <  edge
    --< - Return 1.0 if x >= edge
    --<
    --< @param edge
    --< The edge input value.
    --<
    --< @param x
    --< The parameter to which the step function is applied.
    --<
    --< @return
    --< Returns the step function.
    ----------------------------------------------------------------------------
    function Step (edge, x : in     Vkm_Float) return Vkm_Float is
        (if x < edge then 0.0 else 1.0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unit step function.
    --<
    --< @description
    --< Compute the step function as follows:
    --< - Return 0.0 if x <  edge
    --< - Return 1.0 if x >= edge
    --<
    --< @param edge
    --< The edge input value.
    --<
    --< @param x
    --< The parameter to which the step function is applied.
    --<
    --< @return
    --< Returns the step function.
    ----------------------------------------------------------------------------
    function Step (edge, x : in     Vkm_Double) return Vkm_Double is
        (if x < edge then 0.0 else 1.0) with Inline;


    ----------------------------------------------------------------------------
    --< @summary
    --< Unit step function.
    --<
    --< @description
    --< Apply Step() to each component of input vectors x and edge, returning the
    --< resulting vector.
    ----------------------------------------------------------------------------
    function Step is new GFT.Apply_Func_IV_IV_RV(Step);


    ----------------------------------------------------------------------------
    --< @summary
    --< Unit step function.
    --<
    --< @description
    --< Apply Step() to each component of input vectors x using the scalar edge
    --< for each component. The resulting vector is returned.
    ----------------------------------------------------------------------------
    function Step is new GFT.Apply_Func_IV_IS_RV(Step);


    ----------------------------------------------------------------------------
    --< @summary
    --< Unit step function.
    --<
    --< @description
    --< Apply Step() to each component of input vectors x and edge, returning the
    --< resulting vector.
    ----------------------------------------------------------------------------
    function Step is new GDT.Apply_Func_IV_IV_RV(Step);


    ----------------------------------------------------------------------------
    --< @summary
    --< Unit step function.
    --<
    --< @description
    --< Apply Step() to each component of input vectors x using the scalar edge
    --< for each component. The resulting vector is returned.
    ----------------------------------------------------------------------------
    function Step is new GDT.Apply_Func_IV_IS_RV(Step);


    ----------------------------------------------------------------------------
    --< @summary
    --< Smooth unit step function.
    --<
    --< @description
    --< Compute the smooth step function of x between edge0 and edge1:
    --< - If x is less than edge0, the step is 0. 
    --< - If x is greater than edge1 the step is 1.
    --< - If x is between edge0 and edge1, the step is range [0.0 .. 1.0].
    --<
    --< This algorithm is computed as follows:
    --<     t = clamp ((x - edge0) / (edge1 - edge0), 0, 1).
    --<     t = t^2(3 - 2t)
    --<     return t.
    --<
    ----------------------------------------------------------------------------
    function Smooth_Step is new Vulkan.Math.Numerics.Smooth_Step(Vkm_Float,Clamp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Smooth unit step function.
    --<
    --< @description
    --< Compute the smooth step function of x between edge0 and edge1:
    --< - If x is less than edge0, the step is 0. 
    --< - If x is greater than edge1 the step is 1.
    --< - If x is between edge0 and edge1, the step is range [0.0 .. 1.0].
    --<
    --< This algorithm is computed as follows:
    --<     t = clamp ((x - edge0) / (edge1 - edge0), 0, 1).
    --<     t = t^2(3 - 2t)
    --<     return t.
    --<
    ----------------------------------------------------------------------------
    function Smooth_Step is new Vulkan.Math.Numerics.Smooth_Step(Vkm_Double,Clamp);


    ----------------------------------------------------------------------------
    --< @summary
    --< Smooth unit step function.
    --<
    --< @description
    --< Apply the Smooth_Step() function component-wise on input vectors edge0,
    --< edge1, and x. The resulting vector is returned.
    ----------------------------------------------------------------------------
    function Smooth_Step is new GFT.Apply_Func_IV_IV_IV_RV(Smooth_Step);


    ----------------------------------------------------------------------------
    --< @summary
    --< Smooth unit step function.
    --<
    --< @description
    --< Apply the Smooth_Step() function component-wise on input vector x, using
    --< the input scalars edge0 and edge1 for each component. Return the resulting
    --< vector.
    ----------------------------------------------------------------------------
    function Smooth_Step is new GFT.Apply_Func_IS_IS_IV_RV(Smooth_Step);


    ----------------------------------------------------------------------------
    --< @summary
    --< Smooth unit step function.
    --<
    --< @description
    --< Apply the Smooth_Step() function component-wise on input vectors edge0,
    --< edge1, and x. The resulting vector is returned.
    ----------------------------------------------------------------------------
    function Smooth_Step is new GDT.Apply_Func_IV_IV_IV_RV(Smooth_Step);


    ----------------------------------------------------------------------------
    --< @summary
    --< Smooth unit step function.
    --<
    --< @description
    --< Apply the Smooth_Step() function component-wise on input vector x, using
    --< the input scalars edge0 and edge1 for each component. Return the resulting
    --< vector.
    ----------------------------------------------------------------------------
    function Smooth_Step is new GDT.Apply_Func_IS_IS_IV_RV(Smooth_Step);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds a NaN.
    --<
    --< @description
    --< Determine whether the input holds a NaN. Always returns false.
    ----------------------------------------------------------------------------
    function Is_Nan is new Vulkan.Math.Numerics.Is_Nan(Vkm_Float);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds a NaN.
    --<
    --< @description
    --< Determine whether the input holds a NaN. Always returns false.
    ----------------------------------------------------------------------------
    function Is_Nan is new Vulkan.Math.Numerics.Is_Nan(Vkm_Double);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds a NaN.
    --<
    --< @description
    --< Apply Is_Nan() component-wise to each component of the input vector. The
    --< resulting vector of boolean values is returned.
    ----------------------------------------------------------------------------
    function Is_Nan is new Apply_Func_IVF_RVB(Is_Nan);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds a NaN.
    --<
    --< @description
    --< Apply Is_Nan() component-wise to each component of the input vector. The
    --< resulting vector of boolean values is returned.
    ----------------------------------------------------------------------------
    function Is_Nan is new Apply_Func_IVD_RVB(Is_Nan);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds an Inf.
    --<
    --< @description
    --< Determine whether the input holds an Inf. Always returns false.
    ----------------------------------------------------------------------------
    function Is_Inf is new Vulkan.Math.Numerics.Is_Inf(Vkm_Float);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds an Inf.
    --<
    --< @description
    --< Determine whether the input holds an Inf. Always returns false.
    ----------------------------------------------------------------------------
    function Is_Inf is new Vulkan.Math.Numerics.Is_Inf(Vkm_Double);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds an Inf.
    --<
    --< @description
    --< Apply Is_Inf() component-wise to each component of the input vector. The
    --< resulting vector of boolean values is returned.
    ----------------------------------------------------------------------------
    function Is_Inf is new Apply_Func_IVF_RVB(Is_Inf);


    ----------------------------------------------------------------------------
    --< @summary
    --< Determine whether the input holds an Inf.
    --<
    --< @description
    --< Apply Is_Inf() component-wise to each component of the input vector. The
    --< resulting vector of boolean values is returned.
    ----------------------------------------------------------------------------
    function Is_Inf is new Apply_Func_IVD_RVB(Is_Inf);


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert float bits to int bits.
    --<
    --< @description
    --< Convert the floating point value to a signed integer that
    --< represents the encoding for the floating point value.
    ----------------------------------------------------------------------------
    function Float_Bits_To_Int is new
        Ada.Unchecked_Conversion(Source => Vkm_Float, Target => Vkm_Int);


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert float bits to uint bits.
    --<
    --< @description
    --< Convert the floating point value to an unsigned integer that
    --< represents the encoding for the floating point value.
    ----------------------------------------------------------------------------
    function Float_Bits_To_Uint is new
        Ada.Unchecked_Conversion(Source => Vkm_Float, Target => Vkm_Uint);


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert float bits to int bits.
    --<
    --< @description
    --< Apply Float_Bits_To_Int() to each component of the input vector. The
    --< resulting GenIType vector is returned.
    ----------------------------------------------------------------------------
    function Float_Bits_To_Int is new Apply_Func_IVF_RVI(Float_Bits_To_Int);


    ----------------------------------------------------------------------------
    --< @summary
    --< Convert float bits to uint bits.
    --<
    --< @description
    --< Apply Float_Bits_To_Uint() to each component of the input vector. The
    --< resulting GenUType vector is returned.
    ----------------------------------------------------------------------------
    function Float_Bits_To_Uint is new Apply_Func_IVF_RVU(Float_Bits_To_Uint);


    ----------------------------------------------------------------------------
    --< @brief
    --< Convert the floating point value to a signed or unsigned integer that
    --< represents the encoding for the floating point value.
    --
    --< @param[in]     value The floating point value.
    --
    --< @returns The signed or unsigned integer representation of the float.
    ----------------------------------------------------------------------------
    function Int_Bits_To_Float is new
        Ada.Unchecked_Conversion(Source => Vkm_Int, Target => Vkm_Float);
    function Uint_Bits_To_Float is new
        Ada.Unchecked_Conversion(Source => Vkm_Uint, Target => Vkm_Float);
    function Int_Bits_To_Float is new Apply_Func_IVI_RVF(Int_Bits_To_Float);
    function Uint_Bits_To_Float is new Apply_Func_IVU_RVF(Uint_Bits_To_Float);


    ----------------------------------------------------------------------------
    --< @brief
    --< Compute a fused multiply add operation.
    --
    --< @param[in]     a, b, c The parameters for the computation.
    --
    --< @return a * b + c
    ----------------------------------------------------------------------------
    function Fma(a, b, c : in     Vkm_Float) return Vkm_Float is
        (a * b + c) with Inline;
    function Fma is new GFT.Apply_Func_IV_IV_IV_RV(Fma);
    function Fma(a, b, c : in     Vkm_Double) return Vkm_Double is
        (a * b + c) with Inline;
    function Fma is new GDT.Apply_Func_IV_IV_IV_RV(Fma);


    ----------------------------------------------------------------------------
    --< @brief
    --< Splits the floating point value x into its significand and exponent parts.
    --
    --<     x = significand * 2^exponent
    --
    --< @param[in]     x        The value to split.
    --< @param[out]    exponent The exponent of x.
    --
    --< @return The significand of x.
    ----------------------------------------------------------------------------
    function Frexp is new Vulkan.Math.Numerics.Frexp(Vkm_Float);
    function Frexp is new Vulkan.Math.Numerics.Frexp(Vkm_Double);
    function Frexp is new Apply_Func_IVF_OVI_RVF(Frexp);
    function Frexp is new Apply_Func_IVD_OVI_RVD(Frexp);


    ----------------------------------------------------------------------------
    --< @brief
    --< This operation composes a floting point number from a fraction value and
    --< an exponent value.
    --
    --<     x = significand * 2^exponent
    --
    --< @param[in]     significand The significand.
    --< @param[in]     exponent    The exponent.
    --
    --< @returns x
    ----------------------------------------------------------------------------
    function Ldexp is new Vulkan.Math.Numerics.Ldexp(Vkm_Float);
    function Ldexp is new Vulkan.Math.Numerics.Ldexp(Vkm_Double);
    function Ldexp is new Apply_Func_IVF_IVI_RVF(Ldexp);
    function Ldexp is new Apply_Func_IVD_IVI_RVD(Ldexp);

end Vulkan.Math.Common;
