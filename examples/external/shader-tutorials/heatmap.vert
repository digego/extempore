#version 400

layout(location = 0) in vec2 vertex_position;
layout(location = 1) in vec2 value;
uniform float size;
uniform mat4 model;
out vec2 hmvalue;

void main () {
  hmvalue = value;
  gl_Position = model * vec4(vertex_position,1.0,1.0);
  gl_PointSize = size;
}
