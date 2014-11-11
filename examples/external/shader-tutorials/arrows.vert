#version 400

layout(location = 0) in vec2 vertex_position;

float nx = 512.;

void main () {
  gl_Position = vec4(vertex_position, 1.0, 1.0);
}
