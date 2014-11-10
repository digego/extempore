#version 400

in vec4 colour;
uniform sampler2D texStar;

out vec4 outColor;

void main() {
  outColor = texture(texStar, gl_PointCoord) * colour;
}
