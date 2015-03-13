#version 400

in float alpha;

out vec4 frag_colour;

void main () {
  frag_colour = vec4(alpha);
}
