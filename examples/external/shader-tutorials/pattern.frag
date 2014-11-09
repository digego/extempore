#version 400

in vec3 colour;
out vec4 frag_colour;

void main () {
  frag_colour = vec4(mod(colour.r * 2.0 * gl_FragCoord.x, 1.0),
                     mod(colour.g * 2.3 * gl_FragCoord.y, 1.0),
                     colour.b,
                     1.0);
}
