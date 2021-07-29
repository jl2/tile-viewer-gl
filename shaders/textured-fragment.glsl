#version 410 core

uniform mat4 view_transform;

uniform sampler2D image;

in vec3 normal;
in vec3 position;
in vec2 uv;

out vec4 out_color;

void main() {
     vec4 diffuse_color = texture(image, uv);

     out_color = diffuse_color;
}
