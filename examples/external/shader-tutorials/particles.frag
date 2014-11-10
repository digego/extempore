#version 400

in vec3 colour;

out vec4 outColor;

uniform sampler2D texStar;

void main() {
  outColor = texture(texStar, gl_PointCoord) * vec4(colour, 1.0);
}
