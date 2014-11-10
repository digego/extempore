#version 400

layout(location = 0) in vec2 pvert;
layout(location = 1) in vec2 pvel;

out vec4 colour;

void main() {
  gl_PointSize = max(200.0, 5.*sqrt(length(pvel)));
  colour = vec4(.2,pvel/2.,1.0);
  gl_Position = vec4(pvert,0.0,1.0);
}
