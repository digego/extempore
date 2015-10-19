#version 400

layout(location = 0) in vec2 position;
layout(location = 1) in vec3 velocity;

uniform vec2 nxny;

out vec4 colour;

void main() {
  gl_PointSize = min(50.0, 8.*sqrt(length(velocity)));
  colour = vec4(velocity,1.0);
  gl_Position = vec4((2*(position/nxny))-1.0,0.0,1.0);
}
