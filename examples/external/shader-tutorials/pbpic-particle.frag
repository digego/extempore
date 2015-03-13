#version 400

in vec4 colour;
uniform sampler2D particles_tex;

out vec4 outColor;

void main() {
  outColor = texture(particles_tex, gl_PointCoord) * colour;
}
