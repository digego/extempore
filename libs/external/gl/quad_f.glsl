// start file

#version 330

uniform int override_alpha;
uniform float alpha;

uniform sampler2D tex1;   // tex unit 0
in vec2 UVCoord;
out vec4 xtmColour;

void main() {
  if (override_alpha > 0) {
    xtmColour = vec4(texture(tex1,UVCoord).rgb, alpha);    
  } else {
    xtmColour = texture(tex1,UVCoord);
  }
}

// end file
