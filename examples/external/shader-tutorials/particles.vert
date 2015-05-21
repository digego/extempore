#version 400

layout(location = 0) in vec2 position;
layout(location = 1) in vec2 velocity;

out vec4 colour;

void main() {
  gl_PointSize = min(200.0, 300.*sqrt(length(velocity)));
  colour = vec4(.2,velocity/2.,1.0);
  gl_Position = vec4(position,0.0,1.0);
}
