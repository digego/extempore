#version 400

in vec4 colour;
uniform sampler2D tex;

out vec4 outColor;

void main() {
  outColor = texture(tex, gl_PointCoord) * colour;
}
