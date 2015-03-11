#version 400

in vec4 colour;
uniform sampler2D tex;

out vec4 outColor;

void main() {
  outColor = texture(tex, gl_PointCoord).rrra * colour;
  // outColor = vec4(1,0,1,1); // pink for debugging
}
