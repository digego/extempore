#version 400

// vertex positions input attribute
in vec2 vp;

// per-vertex texture coordinates input attribute
in vec2 vt;

// texture coordinates to be interpolated to fragment shaders
out vec2 st;

void main () {
  // interpolate texture coordinates
  st = vt;
  // transform vertex position to clip space (camera view and perspective)
  gl_Position = vec4 (vp, 0.0, 1.0);
}
