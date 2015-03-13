#version 400

layout(location = 0) in vec2 vertex_position;

out float alpha;

void main () {
  alpha = mod(gl_VertexID, 2);
  gl_Position = vec4(vertex_position, 1.0, 1.0);
}
