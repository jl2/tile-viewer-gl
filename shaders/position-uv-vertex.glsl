#version 410 core

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec2 in_uv;

uniform mat4 view_transform;

out vec2 uv;

void main()
{
     mat4 final_transform = view_transform;
     vec4 pos4 = final_transform * vec4(in_position, 1.0);

     gl_Position = pos4;
     uv = in_uv;
}
