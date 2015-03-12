#version 400

in vec3 f_colour;
out vec4 frag_colour;

void main() {
	frag_colour = vec4 (f_colour, 1.0);
}
