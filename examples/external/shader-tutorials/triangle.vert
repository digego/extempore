#version 400

layout(location = 0) in vec3 vertex_position;
layout(location = 1) in vec3 vertex_colour;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

out vec3 colour;

void main () {
  colour = vertex_colour;
  // gl_Position = vec4(vertex_position, 1.0);
  gl_Position = proj * view * model * vec4(vertex_position, 1.0);
}
