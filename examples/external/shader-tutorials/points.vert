#version 400

layout(location = 0) in vec2 point_position;
layout(location = 1) in vec3 point_colour;

// uniform mat4 model;
// uniform mat4 view;
// uniform mat4 proj;

out vec3 colour;

void main () {
  colour = point_colour;
  gl_Position = vec4(point_position, 0.0, 1.0);
  gl_PointSize = 100.;
  // gl_Position = proj * view * model * vec4(point_position, 1.0);
}
