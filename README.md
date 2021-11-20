# VulkanAda
An attempt at creating a binding to the Vulkan Library for the Ada Programming Language.

Addtionally, vector and matrix types compatible with the thick Vulkan binding are provided.

The vector and matrix types and functions may eventually be moved to its own repository since it isn't directly related to Vulkan, but for now I am content with providing it with the eventual Vulkan binding.

API documentation is generated from the Ada spec files (.ads) and is published to Github-Pages at the following site:
https://zrmyers.github.io/VulkanAda/docs/

## Dependencies
This project depends on the following:
- [GLFW](https://www.glfw.org/)
- [AdaCore GNAT toolset](https://www.adacore.com/download)
- [Vulkan](https://www.khronos.org/vulkan/)
- [GLFWAda](https://github.com/zrmyers/GLFWAda)

On the Arch Linux operating system, the AdaCore GNAT toolset was obtained through the Arch User Repository (AUR), and GLFW and Vulkan were installed through the Arch Linux package manager.

GLFWAda was obtained and installed from another git repository, linked to above.

## Roadmap
The following is a roadmap for this repository:

|Milestone                                                          |  Description                         | Status     |
|-------------------------------------------------------------------|--------------------------------------|------------|
|[VulkanAda 0.0.2](https://github.com/zrmyers/VulkanAda/milestone/2)| GLSL-like matrix types and functions |WIP         |
|[VulkanAda 0.0.3](https://github.com/zrmyers/VulkanAda/milestone/3)| GLSL Extension-like Matrix transforms|Not Started |
|[VulkanAda 0.1.0](https://github.com/zrmyers/VulkanAda/milestone/4)| Vulkan Instance functions and tests. |Not Started |
|[VulkanAda 0.1.1](https://github.com/zrmyers/VulkanAda/milestone/5)| Vulkan Device functions and tests.   |Not Started |

The following milestones have been completed:

|Milestone                                                          |  Description                         |
|-------------------------------------------------------------------|--------------------------------------|
|[VulkanAda 0.0.1](https://github.com/zrmyers/VulkanAda/milestone/1)| GLSL-like vector types and functions |
