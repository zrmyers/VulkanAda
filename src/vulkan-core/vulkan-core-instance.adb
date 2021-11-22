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
with Vulkan.Core.Vulkan_Core_H; use Vulkan.Core.Vulkan_Core_H;
with stdint_h;                  use stdint_h;
with Interfaces.C;              use Interfaces.C;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Ada.Containers;            use Ada.Containers;
with System;                    use System;


--------------------------------------------------------------------------------
--< @group Vulkan Core
--------------------------------------------------------------------------------
--< @summary
--< This package provides access to a Vulkan instance.
--------------------------------------------------------------------------------
package body Vulkan.Core.Instance is

    type Extension_Properties_Array is array (Positive range <>) of aliased VkExtensionProperties;

    type Layer_Properties_Array is array (Positive range <>) of aliased VkLayerProperties;

    type Chars_Ptr_Array is array (Natural range <>) of Interfaces.C.Strings.chars_ptr;


    --< Convert Extension properties to a type that is easier to use.
    function To_Vk_Extension_Properties(from : in     VkExtensionProperties)
        return Vk_Extension_Properties;

    --< Convert Layer properties t a type that is easier to use in Ada.
    function To_Vk_Layer_Properties(from : in     VkLayerProperties)
        return Vk_Layer_Properties;

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
    procedure Vk_Enumerate_Instance_Extension_Properties (
        properties : in out Vk_Extension_Properties_Vector) is

        extension_count : aliased stdint_h.uint32_t := 0;
        result : VkResult;
    begin

        result := vkEnumerateInstanceExtensionProperties(
            Interfaces.C.Strings.Null_Ptr,
            extension_count'access,
            null);
        if (result = VK_SUCCESS) and (extension_count > 0) then
            declare

                extension_properties : aliased Extension_Properties_Array(1 .. Positive(extension_count)) :=
                    (others => (extensionName => (others => nul), specVersion => 0));
            begin

                result := vkEnumerateInstanceExtensionProperties(
                    Interfaces.C.Strings.Null_Ptr,
                    extension_count'access,
                    extension_properties(1)'Access);

                if result = VK_SUCCESS then
                    -- Copy to Vector.
                    for property_index in 1 .. Positive(extension_count) loop

                        properties.Append(To_Vk_Extension_Properties(extension_properties(property_index)));
                    end loop;
                end if;
            end;
        end if;

        if result /= VK_SUCCESS then
            raise VULKAN_ERROR with "vkEnumerateInstanceExtensionProperties failed with result " & result'Image;
        end if;

    end Vk_Enumerate_Instance_Extension_Properties;


    ----------------------------------------------------------------------------


    procedure Vk_Enumerate_Instance_Layer_Properties (
        properties : in out Vk_Layer_Properties_Vector) is

        layer_count : aliased stdint_h.uint32_t := 0;
        result : VkResult;

    begin

        result := vkEnumerateInstanceLayerProperties(
            layer_count'access,
            null);

        if (result = VK_SUCCESS) and (layer_count > 0) then
            declare

                layer_properties : aliased Layer_Properties_Array(1 .. Positive(layer_count)) :=
                    (others => (layerName => (others => nul),
                                specVersion => 0,
                                implementationVersion => 0,
                                description => (others => nul)));
            begin

                result := vkEnumerateInstanceLayerProperties(
                    layer_count'access,
                    layer_properties(1)'Access);

                if result = VK_SUCCESS then
                    -- Copy to Vector.
                    for property_index in 1 .. Positive(layer_count) loop

                        properties.Append(To_Vk_Layer_Properties(layer_properties(property_index)));
                    end loop;
                end if;
            end;
        end if;

        if result /= VK_SUCCESS then
            raise VULKAN_ERROR with "vkEnumerateInstanceExtensionProperties failed with result " & result'Image;
        end if;
    end Vk_Enumerate_Instance_Layer_Properties;


    ----------------------------------------------------------------------------


    function Vk_Create_Instance(
        create_info : in     Vk_Instance_Create_Info) return Vk_Instance is

        application_info : VkApplicationInfo;
        layer_count : constant Natural := Natural(create_info.enabled_layer_names.Length);
        pp_layer_names : Chars_Ptr_Array(0 ..  layer_count);
        extension_count : constant Natural := Natural(create_info.enabled_extension_names.Length);
        pp_extension_names : Chars_Ptr_Array(0 .. extension_count);

        local_create_info : VkInstanceCreateInfo; -- Allow default initialization.
        instance : Vk_Instance;
        result : VkResult;
    begin

        -- Fill out application_info
        application_info.pApplicationName :=
            New_String(To_String(create_info.application_info.application_name));
        application_info.applicationVersion :=
            To_Uint32(create_info.application_info.application_version);
        application_info.pEngineName :=
            New_String(To_String(create_info.application_info.engine_name));
        application_info.engineVersion :=
            To_Uint32(create_info.application_info.engine_version);
        application_info.apiVersion :=
            To_Uint32(create_info.application_info.api_version);
        local_create_info.pApplicationInfo := application_info'Address;

        -- Fill out layers to be enabled.
        if layer_count > 0 then
            for index in 0 .. layer_count - 1 loop

                declare
                    layer_name : constant String := To_String(create_info.enabled_layer_names(index));
                begin
                    pp_layer_names(index) := Interfaces.C.Strings.New_String(layer_name);
                end;
            end loop;

            local_create_info.enabledLayerCount := Interfaces.C.unsigned(layer_count);
            local_create_info.ppEnabledLayerNames := pp_layer_names'Address;
        end if;

        -- Fill out extensions to be anabled.
        if extension_count > 0 then
            -- Fill out the char_ptr_array
            for index in 0 .. extension_count - 1 loop
                declare
                    extension_name : constant String := To_String(create_info.enabled_extension_names(index));
                begin
                    pp_extension_names(index) := Interfaces.C.Strings.New_String(extension_name);
                end;
            end loop;

            local_create_info.enabledExtensionCount := Interfaces.C.unsigned(extension_count);
            local_create_info.ppEnabledExtensionNames := pp_extension_names'Address;
        end if;

        result := vkCreateInstance(
            pCreateInfo => local_create_info'Address,
            pInstance   => instance.instance'Address);

        if result /= VK_SUCCESS then
            raise VULKAN_ERROR with "Call to vkCreateInstance() failed with result " & result'Image;
        end if;
        return instance;
    end Vk_Create_Instance;


    ----------------------------------------------------------------------------


    procedure Vk_Destroy_Instance(instance : in out Vk_Instance) is begin

        vkDestroyInstance(vkInstance(instance.instance));

        instance.instance := System.Null_Address;

    end Vk_Destroy_Instance;


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


    ----------------------------------------------------------------------------


    function To_Vk_Layer_Properties(from : in     VkLayerProperties)
        return Vk_Layer_Properties is

        to : Vk_Layer_Properties;
    begin

        to.name := To_Vk_String(To_Ada(from.layerName));
        to.spec_version := To_Vk_Spec_Version(from.specVersion);
        to.implementation_version := To_Vk_Spec_Version(from.implementationVersion);
        to.description := To_Vk_String(To_Ada(from.layerName));

        return to;

    end To_Vk_Layer_Properties;

end Vulkan.Core.Instance;
