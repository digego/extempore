#version 400

uniform vec3      iResolution;           // viewport resolution (in pixels)
uniform float     iGlobalTime;           // shader playback time (in seconds)
// uniform float     iChannelTime[4];       // channel playback time (in seconds)
// uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)
// uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
// uniform samplerXX iChannel03;            // input channel. XX = 2D/Cube
// uniform vec4      iDate;                 // (year, month, day, time in seconds)
uniform float     iSampleRate;           // sound sample rate (i.e., 44100)

out vec4 fragColor;

void main () {
  // License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
  // Created by S.Guillitte 
  float k=0.;
  vec3 d = vec3(gl_FragCoord)/iResolution-.5;
  vec3 o = d;
  vec3 c = k*d;
  vec3 p;
    
  for( int i=0; i<99; i++ ){
        
    p = o+sin(iGlobalTime*.1);
		for (int j = 0; j < 10; j++) 
		
      p = abs(p.zyx-.4) -.7,k += exp(-6. * abs(dot(p,o)));
		
		k/=3.;
    o += d *.05*k;
    c = .97*c + .1*k*vec3(k*k,k,1);
  }
  c =  .4 *log(1.+c);
  fragColor.rgb = c;
}
