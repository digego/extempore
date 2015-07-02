#version 400

layout(location = 0) in vec2 vertex_position;
layout(location = 1) in vec3 vertex_colour;

// uniform mat4 model_mat;
uniform mat4 view_mat;
// uniform mat4 proj_mat;

out vec3 colour;

void main () {
  colour = vertex_colour;
  gl_Position = view_mat * vec4(vertex_position, 0.0, 1.0);
}
