#version 400

in vec2 Texcoord;

out vec4 outColor;

uniform sampler2D texStar;

void main() {
  outColor = vec4(Texcoord,1.0,1.0);
  // outColor = texture(texStar, Texcoord);
}
