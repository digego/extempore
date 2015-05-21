#version 400

in vec2 tex_coord;
out vec4 frag_colour;

uniform sampler2D tex;

void main()
{
  frag_colour = texture(tex, tex_coord);
}
