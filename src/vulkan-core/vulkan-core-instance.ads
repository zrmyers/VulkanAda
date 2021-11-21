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
with Ada.Containers.Vectors;

--------------------------------------------------------------------------------
--< @group Vulkan Core
--------------------------------------------------------------------------------
--< @summary
--< This package provides access to a Vulkan instance.
--------------------------------------------------------------------------------
package Vulkan.Core.Instance is

    --< This record describes an instance of
    type Vk_Extension_Properties is record

       --< Name of the extension.
       name : Vk_String;

       --< The the version of the extension.
       version : Vk_Spec_Version;
    end record;

    package Vk_Extension_Properties_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Vk_Extension_Properties);

    subtype Vk_Extension_Properties_Vector is Vk_Extension_Properties_Vectors.Vector;

    ----------------------------------------------------------------------------
    --< @brief
    --< This operation retrieves all of the extension properties that can be used
    --< to create a Vulkan instance.
    --<
    --< @param properties
    --< A list of properties with which an instance of Vulkan can be created.
    --<
    --< @errors
    --< The following exceptions can be raised by this operation:
    --<     VULKAN_ERROR
    ----------------------------------------------------------------------------
    procedure Enumerate_Extension_Properties (
        properties : in out Vk_Extension_Properties_Vector);

    function Image(property : in     Vk_Extension_Properties) return String is
        ("[ name = " & Image(property.name) & "; version = " & Image(property.version) & " ]") with inline;

end Vulkan.Core.Instance;
