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
with Vulkan.Core.Physical_Device; use Vulkan.Core.Physical_Device;

--------------------------------------------------------------------------------
--< @group Vulkan Core
--------------------------------------------------------------------------------
--< @summary
--< This package provides access to a Vulkan instance.
--------------------------------------------------------------------------------
package Vulkan.Core.Instance is

    --< Instance extension names.
    VK_EXT_debug_utils : constant Vk_String := To_Vk_String("VK_EXT_debug_utils");

    --< Instance layer names.
    VK_LAYER_KHRONOS_validation : constant Vk_String := To_Vk_String("VK_LAYER_KHRONOS_validation");

    --< A reference to the vkInstance type.
    type Vk_Instance is new Vk_Object;

    --< This record describes extensions that can be supported by an instance of
    --< Vulkan on the platform on which it is being run.
    type Vk_Extension_Properties is record

       --< Name of the extension.
       name : Vk_String;

       --< The the version of the extension.
       version : Vk_Spec_Version;
    end record;

    --< Instantiation of Vectors package for holding a list of extensions.
    package Vk_Extension_Properties_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Vk_Extension_Properties);

    --< Subtype declared for the extension properties vector to make it easier to
    --< use.
    subtype Vk_Extension_Properties_Vector is Vk_Extension_Properties_Vectors.Vector;

    --< This record supported layer properties for an instant of Vulkan.
    type Vk_Layer_Properties is record
        name : Vk_String;
        spec_version : Vk_Spec_Version;
        implementation_version : Vk_Spec_Version;
        description : Vk_String;
    end record;

    package Vk_Layer_Properties_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Vk_Layer_Properties);

    subtype Vk_Layer_Properties_Vector is Vk_Layer_Properties_Vectors.Vector;

    --< Application creation information.
    type Vk_Application_Create_Info is record
        application_name : Vk_String := To_Vk_String("");
        application_version : Vk_Spec_Version := (major => 0, minor => 0, patch => 0);
        engine_name : Vk_String := To_Vk_String("");
        engine_version : Vk_Spec_Version := (major => 0, minor => 0, patch => 0);
        api_version : Vk_Spec_Version := (major => 1, minor => 2, patch => 0);
    end record;

    --< Instance creation information.
    type Vk_Instance_Create_Info is record
        application_info        : Vk_Application_Create_Info;
        enabled_layer_names     : Vk_String_Vector;
        enabled_extension_names : Vk_String_Vector;
    end record;

    ----------------------------------------------------------------------------
    -- Operations
    ----------------------------------------------------------------------------
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
        properties : in out Vk_Extension_Properties_Vector);


    ----------------------------------------------------------------------------
    --< @brief
    --< This operation retrieves all of the layer properties that can be used
    --< to create a Vulkan instance.
    --<
    --< @param properties
    --< A list of properties with which an instance of Vulkan can be created.
    --<
    --< @errors
    --< The following exceptions can be raised by this operation:
    --<     VULKAN_ERROR
    ----------------------------------------------------------------------------
    procedure Vk_Enumerate_Instance_Layer_Properties (
        properties : in out Vk_Layer_Properties_Vector);


    ----------------------------------------------------------------------------
    --< @brief
    --< This operation creates a new Vulkan Instance.
    --<
    --< @param create_info
    --< Information for creating a new Vulkan Instance.
    --<
    --< @return
    --< Returns a handle to the Vulkan Instance.
    --<
    --< @errors
    --< The following exceptions can be raised by this operation:
    --<     VULKAN_ERROR
    ----------------------------------------------------------------------------
    function Vk_Create_Instance (
        create_info : in     Vk_Instance_Create_Info) return Vk_Instance;


    ----------------------------------------------------------------------------
    --< @brief
    --< This operation destroys a Vulkan instance.
    --<
    --< @param instance
    --< The instance to destroy.
    --<
    --< @errors
    --< The following exceptions can be raised by this operation:
    --<     VULKAN_ERROR
    ----------------------------------------------------------------------------
    procedure Vk_Destroy_Instance (
        instance : in out Vk_Instance);


    ----------------------------------------------------------------------------
    --< @brief
    --< This operation enumerates all physical devices that can be used from the instance.
    --<
    --< @param physical_devices
    --< A list of physical devices that were found by the Vulkan Instance.
    --<
    ----------------------------------------------------------------------------
    procedure Vk_Enumerate_Physical_Devices (
        instance         : in     Vk_Instance;
        physical_devices : in out Vk_Physical_Device_Vector);


    ----------------------------------------------------------------------------
    --< @brief
    --< This operation converts the extension properties object to a human readable
    --< string.
    function Image (property : in     Vk_Extension_Properties) return String is
        ("[ name = " & Image(property.name) & ", version = " & Image(property.version) & " ]") with inline;

    function Image (property : in     Vk_Layer_Properties) return String is
        ("[ name = " & Image(property.name) &
         ", spec_version = " & Image(property.spec_version) &
         ", implementation_version = " & Image(property.implementation_version) &
         ", description = " & Image(property.description) & "]") with inline;

    function Image (application_info :in     Vk_Application_Create_Info) return String is
        ("[ application_name = "    & Image(application_info.application_name) & LF &
         ", application_version = " & Image(application_info.application_version) & LF &
         ", engine_name = "         & Image(application_info.engine_name) & LF &
         ", engine_version = "      & Image(application_info.engine_version) & LF &
         ", api_version = " & Image(application_info.api_version) & " ]") with inline;

    function Image (instance_create_info : in     Vk_Instance_Create_Info) return String is
        ("[ application_info = " & Image(instance_create_info.application_info) & LF &
         ", enabled_extension_names.Length = " & instance_create_info.enabled_extension_names.Length'Image & LF &
         ", enabled_layer_names.Length = "  & instance_create_info.enabled_layer_names.Length'Image & "]") with inline;

end Vulkan.Core.Instance;
