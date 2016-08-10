// start file

#version 330

uniform sampler2D tex1;
in vec2 UVCoord;
out vec4 xtmColour;

void main() {
   xtmColour = texture(tex1,UVCoord);
}

// end file
