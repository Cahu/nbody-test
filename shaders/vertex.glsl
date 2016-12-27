#version 330

layout(location = 0) in vec4 position;

void main()
{
    gl_Position = vec4(position.xyz / 2000, 1.0);
}
