#version 400

layout(location = 0) in vec2 vp;
layout(location = 1) in vec2 tc;
out vec2 tex_coord;

void main () {
  // Colour = vc;
  gl_Position = vec4(vp, 0.0, 1.0);
  tex_coord = tc;
}
