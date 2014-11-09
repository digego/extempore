#version 400

layout(location = 0) in vec2 position;
layout(location = 1) in vec2 texcoord;

out vec2 Texcoord;

void main() {
  Texcoord = texcoord;
  gl_Position = vec4(position, 0.0, 1.0);
  gl_PointSize = 100.0;
}
