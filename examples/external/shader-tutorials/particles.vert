#version 400

layout(location = 0) in vec2 position;
layout(location = 1) in vec3 vert_colour;

out vec3 colour;

void main() {
  colour = vert_colour;
  gl_Position = vec4(position, 0.0, 1.0);
  gl_PointSize = 300.0;
}
