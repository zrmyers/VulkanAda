--------------------------------------------------------------------------------
-- MIT License
--
-- Copyright (c) 2021 Zane Myers
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
with stdint_h;
with Ada.Unchecked_Conversion;
with Ada.Strings.Bounded;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;
with Interfaces.C;

--------------------------------------------------------------------------------
--< @group Vulkan Core
--------------------------------------------------------------------------------
--< @summary
--< This package encompasses the core Vulkan API via its subpackages.
--------------------------------------------------------------------------------
package Vulkan.Core is
    pragma Preelaborate;

    --< An exception that can be thrown during runtime.
    VULKAN_ERROR : exception;

    --< Local definition of line feed.
    LF : constant String := "" & Ada.Characters.Latin_1.LF;

    --< Vulkan Specification Major Version Number.
    type Vk_Spec_Major is mod (2 ** 10);

    --< Vulkan Specification Minor Version Number.
    type Vk_Spec_Minor is mod (2 ** 10);

    --< Vulkan Specification Patch Version Number.
    type Vk_Spec_Patch is mod (2 ** 12);

    --< Vulkan Specification Version Number.
    type Vk_Spec_Version is record
        major : Vk_Spec_Major;
        minor : Vk_Spec_Minor;
        patch : Vk_Spec_Patch;
    end record;

    --< Describes bits used to pack or unpack data from the record.
    for Vk_Spec_Version use
        record
            major at 0 range 22  .. 31;
            minor at 0 range 12 .. 21;
            patch at 0 range 0 .. 11;
        end record;

    for Vk_Spec_Version'Size use Interfaces.C.unsigned'Size;

    --< Common string type used throughout Vulkan interface.
    package Vk_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 256);

    type Vk_String is new Vk_Strings.Bounded_String;

    package Vk_String_Vectors is new Ada.Containers.Vectors (
        Index_Type   => Natural,
        Element_Type => Vk_String);

    subtype Vk_String_Vector is Vk_String_Vectors.Vector;

    --< Useful function to convert an unsigned 32-bit number into a version
    --< record.
    function To_Vk_Spec_Version is new Ada.Unchecked_Conversion (
        Source => stdint_h.uint32_t,
        Target => Vk_Spec_Version);

    function To_Uint32 is new Ada.Unchecked_Conversion (
        Source => Vk_Spec_Version,
        Target => stdint_h.uint32_t);

    function To_Vk_String(from : in     String) return Vk_String is
        (Vk_String(Vk_Strings.To_Bounded_String(from)));

    function Image(from : in    Vk_Spec_Version) return String is
        (from.major'Image & "." & from.minor'Image & "." & from.patch'Image);

    function Image(string_ref : in     Vk_String) return String is
        (To_String(string_ref));

end Vulkan.Core;
