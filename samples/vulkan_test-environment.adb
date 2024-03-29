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
with Ada.Text_IO;
With Glfw;
with Vulkan; use Vulkan;
With Vulkan.Math.Mat4x4;
with Vulkan.Math.Vec4;
with Vulkan.Math.GenFMatrix; use Vulkan.Math.GenFMatrix;
with Vulkan.Core.Instance;   use Vulkan.Core.Instance;
with Vulkan.Core;            use Vulkan.Core;


procedure Vulkan_Test.Environment is

    hints : constant Glfw.Record_Window_Hints := (
        Client_Api => Glfw.NO_API,
        others     => <>);

    window_handle : Glfw.Glfw_Window := Glfw.No_Window;

    matrix : Vulkan.Math.Mat4x4.Vkm_Mat4 := Vulkan.Math.Mat4x4.Make_Mat4x4;
    vector : Vulkan.Math.Vec4.Vkm_Vec4 := Vulkan.Math.Vec4.Make_Vec4;

    required_extension_names : Glfw.Glfw_String_Vector;

    extension_properties : Vulkan.Core.Instance.Vk_Extension_Properties_Vector;

    instance_info : Vk_Instance_Create_Info :=
        (application_info => (
             application_name => To_Vk_String("Hello Vulkan!"),
             others => <>),
         others => <>);

    instance : Vk_Instance;

begin
    Ada.Text_IO.Put_Line("Initializing Window System");

   -- Initialize GLFW
    GLFW.Platform_Init;

    -- Initialize Window
    GLFW.Window_Set_Hints(hints => hints);

    window_handle := Glfw.Window_Create(
        width  => 1024,
        height => 768,
        title  => "Hello Vulkan!");

    -- Demonstrate VulkanAda
    Ada.Text_IO.Put_Line("VulkanAda Version is " & Vulkan.VKADA_API_VERSION);

    -- Demonstrate vectors
    vector.x(1.0);
    vector.w(1.0);

    Ada.Text_IO.Put_Line(vector.Image);

    matrix := 2.0 * matrix;

    Ada.Text_IO.Put_Line(matrix.Image);

    vector := matrix * vector;

    Ada.Text_IO.Put_Line(vector.Image);

    Vk_Enumerate_Instance_Extension_Properties(extension_properties);

    for property of extension_properties loop

        Ada.Text_IO.Put_Line("Supported Extension: " & Image(property));

    end loop;

    Glfw.Get_Required_Instance_Extensions(required_extension_names);

    for name of required_extension_names loop

        -- Determine whether the name exists in the set of supported extensions
        declare
            required_name : constant Vk_String := Vulkan.Core.To_Bounded_String(Glfw.To_String(name));
            is_supported : Boolean := False;
        begin

            for supported_property of extension_properties loop

                if supported_property.name = required_name then
                    is_supported := True;
                end if;

                exit when is_supported;

            end loop;

            if not is_supported then

                Ada.Text_IO.Put_Line("Required Extension, " & Glfw.To_String(name) & " is NOT supported!");
            else
                instance_info.enabled_extension_names.Append(required_name);
                Ada.Text_IO.Put_Line("Required Extension, " & Glfw.To_String(name) & " is supported!");
            end if;
        end;

    end loop;

    Ada.Text_IO.Put_Line(Image(instance_info));

    Ada.Text_IO.Put_Line("Creating Vulkan Instance");

    -- Create an instance!
    instance := Vk_Create_Instance(create_info => instance_info);

    -- Main Loop
    loop

        ------------------------------------------------------------------------
        -- This loop will run forever until the user closes the window or some
        -- kind of exception occurs, which causes the program to begin handling
        -- the exception.
        ------------------------------------------------------------------------
        pragma Warnings (Off, "variable ""window_handle"" is not modified in loop body");
        pragma Warnings (Off, "possible infinite loop");

        exit when Glfw.Window_Should_Close(window_handle => window_handle);

        pragma Warnings (On, "variable ""window_handle"" is not modified in loop body");
        pragma Warnings (On, "possible infinite loop");

        -- Poll for Glfw events
        Glfw.Platform_Process_Events;
    end loop;

    Ada.Text_IO.Put_Line("Destroying Vulkan Instance");

    Vk_Destroy_Instance(instance => instance);

    Ada.Text_IO.Put_Line("Destroying Window");

    Glfw.Window_Destroy(window_handle => window_handle);

    Ada.Text_IO.Put_Line("Shutting Down GLFW");

    -- Shut down the GLFW instance
    Glfw.Platform_Shutdown;

end Vulkan_Test.Environment;
