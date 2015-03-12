#version 400

layout(points) in;
layout(line_strip, max_vertices = 2) out;

in vec3 vertex_colour[]; // Output from vertex shader for each vertex
out vec3 f_colour; // Output to fragment shader

void main() {
  f_colour= vertex_colour[0]; // Point has only one vertex

  gl_Position = gl_in[0].gl_Position + vec4(-0.1, 0.1, 0.0, 0.0);
  EmitVertex();
  
  gl_Position = gl_in[0].gl_Position + vec4(0.1, 0.1, 0.0, 0.0);
  EmitVertex();
  
  EndPrimitive();
}
