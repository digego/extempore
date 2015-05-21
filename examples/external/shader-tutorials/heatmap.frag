#version 400

out vec4 frag_colour;

vec3 HUEtoRGB(float H){
  float R = abs(H * 6. - 3.) - 1.;
  float G = 2 - abs(H * 6. - 2.);
  float B = 2 - abs(H * 6. - 4.);
  return clamp(vec3(R,G,B),0.0,1.0);
}

vec3 HSLtoRGB(vec3 HSL){
  vec3 RGB = HUEtoRGB(HSL.x);
  float C = (1. - abs(2. * HSL.z - 1.)) * HSL.y;
  return (RGB - 0.5) * C + HSL.z;
}

void main () {
  frag_colour = vec4(HUEtoRGB(mod(cos(50.*gl_FragCoord.x/1680),1.0)), 1.0);
}
