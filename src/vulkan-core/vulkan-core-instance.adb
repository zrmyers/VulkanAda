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
with Vulkan.Vulkan_Core_H; use Vulkan.Vulkan_Core_H;
with stdint_h;             use stdint_h;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Containers;       use Ada.Containers;

--------------------------------------------------------------------------------
--< @group Vulkan Core
--------------------------------------------------------------------------------
--< @summary
--< This package provides access to a Vulkan instance.
--------------------------------------------------------------------------------
package body Vulkan.Core.Instance is

    type Extension_Properties_Array is array (Positive range <>) of aliased VkExtensionProperties;

    --< Convert Extension properties to a type that is easier to use.
    function To_Vk_Extension_Properties(from : in     VkExtensionProperties)
        return Vk_Extension_Properties;

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
        properties : in out Vk_Extension_Properties_Vector) is

        layer_count : aliased stdint_h.uint32_t := 0;
        result : VkResult;
    begin

        result := vkEnumerateInstanceExtensionProperties(
            Interfaces.C.Strings.Null_Ptr,
            layer_count'access,
            null);
        if (result = VK_SUCCESS) and (layer_count > 0) then
            declare

                extension_properties : aliased Extension_Properties_Array(1 .. Positive(layer_count)) :=
                    (others => (extensionName => (others => nul), specVersion => 0));
            begin

                result := vkEnumerateInstanceExtensionProperties(
                    Interfaces.C.Strings.Null_Ptr,
                    layer_count'access,
                    extension_properties(1)'Access);

                if result = VK_SUCCESS then
                    -- Copy to Vector.
                    for property_index in 1 .. Positive(layer_count) loop

                        properties.Append(To_Vk_Extension_Properties(extension_properties(property_index)));
                    end loop;
                end if;
            end;
        end if;

        if result /= VK_SUCCESS then
            raise VULKAN_ERROR with "vkEnumerateInstanceExtensionProperties failed with result " & result'Image;
        end if;

    end Enumerate_Extension_Properties;


    ----------------------------------------------------------------------------
    -- Local Operations
    ----------------------------------------------------------------------------


    function To_Vk_Extension_Properties(from : in     VkExtensionProperties)
        return Vk_Extension_Properties is

        to : Vk_Extension_Properties;

    begin
        to.name := To_Vk_String(To_Ada(from.extensionName));
        to.version := To_Vk_Spec_Version(from.specVersion);

        return to;
    end To_Vk_Extension_Properties;


end Vulkan.Core.Instance;
