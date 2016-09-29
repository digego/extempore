// start file

#version 330

uniform samplerCube tex1;   // tex unit 0
in vec3 UVWCoord;
out vec4 xtmColour;

void main() {
   xtmColour = texture(tex1,UVWCoord);
}

// end file
