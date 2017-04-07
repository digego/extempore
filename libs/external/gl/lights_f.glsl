// start file

#version 330

in vec3 N, E, V;
in vec3 L[5];
in float D[5];

uniform vec4 LightAmbient[5];
uniform vec4 LightDiffuse[5];
uniform vec4 LightSpecular[5];
uniform float ConstantAttenuation[5];
uniform float LinearAttenuation[5];
uniform float QuadraticAttenuation[5];
uniform float SpotAngle[5];
uniform vec4 SpotDir[5];
uniform float SpotExponent[5];

uniform int numLights;
uniform int instances;

uniform vec4 CameraDir;
uniform float MaterialShininess;
uniform vec4 MaterialAmbient;
uniform vec4 MaterialDiffuse;
uniform vec4 MaterialSpecular;
uniform vec4 MaterialEmissive;

uniform mat4 ModelMatrix;
uniform mat4 ViewMatrix;
uniform mat4 ProjectionMatrix;
uniform mat3 NormalMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 ModelViewProjectionMatrix;

uniform sampler2D shadowMap;         // unit 0
uniform sampler2D diffuseTexture;    // unit 1
uniform samplerCube envMap;          // unit 2
uniform sampler2D projectionTexture; // unit 3
//uniform sampler2D heightTexture;
//uniform sampler2D specularTexture;
//uniform sampler2D normalsTexture;


uniform int isEnvMapped;
uniform int isTextured;
uniform int isBumpMapped;
uniform int isDiffuseMapped;
uniform int isSpecularMapped;
uniform int isProjectionTextured;
uniform int isPoints;
uniform int emitVColour;
uniform float envMapWeight;
uniform float projectionTextureWeight;

in vec4 lightVertexPosition[5];
in vec3 UVWCoord;
in vec4 UVWCoordProjectionTexture;
in vec4 vColour;

out vec4 xtmOutColour;

float calcAttenuation(int idx) {
  vec3 LL = normalize(L[idx]); // light vector
  float attenuation = 1.0 / (ConstantAttenuation[idx] + (LinearAttenuation[idx] * D[idx]) + (QuadraticAttenuation[idx] * D[idx] * D[idx]));
  float spotDot = dot(-LL, normalize(SpotDir[idx].xyz));
  float spotAttenuation = 0.0;
  if (spotDot >= SpotAngle[idx]) {
   spotAttenuation = pow(spotDot, SpotExponent[idx]);
  }
  attenuation *= spotAttenuation;
  return attenuation;
}

float calcShadow(int idx) {
  //calc shadows
  float j = 0.5 + float(idx);
  float shadowValue = 0.0;
  vec4 lightVertexPosition2 = lightVertexPosition[idx];
  lightVertexPosition2 /= lightVertexPosition2.w;

  float aa = mod((j-0.5),4.0);
  float bb = floor((j-0.5)/4.0);
  vec2 offset = vec2(0.25*aa,0.25*bb);

  // softer shadowing by adding dither
  for(float x=-0.0004; x<=0.0004; x+=0.0002) {
    for(float y=-0.0004; y<=0.0004; y+=0.0002) {
      if(texture(shadowMap,(lightVertexPosition2.xy*0.25)+offset+vec2(x,y)).r >= lightVertexPosition2.z)
        shadowValue+=1.0;
    }
  }
  shadowValue/=16.0;
  return shadowValue;
}

vec4 calcFrag(int idx, vec3 NN, vec3 EE, float attenuation, float shadowValue) {
  vec3 reflected, tmp1, tmp2;
  vec3 LL = normalize(L[idx]); // light vector
  vec4 outcolor, ambient, diffuse, specular, texcolour, emissive;
  vec3 HV = normalize(LL+EE); // half vector
  float pf = 0.0;
  float nDotLL = max(0.0, dot(NN,LL));
  float nDotHV = max(0.0, dot(NN,HV));

  if(nDotLL > 0.0) {
    pf = pow(nDotHV, MaterialShininess);
  }

  ambient  = vec4(LightAmbient[idx].xyz * MaterialAmbient.xyz * vColour.xyz * attenuation,1.0);
  diffuse  = vec4(LightDiffuse[idx].xyz * MaterialDiffuse.xyz * vColour.xyz * attenuation * nDotLL,1.0);
  specular = vec4(LightSpecular[idx].xyz * MaterialSpecular.xyz * attenuation * pf,1.0);

  if(isTextured > 0) {
    texcolour = diffuse * texture(diffuseTexture,UVWCoord.xy);
    outcolor = vec4(texcolour.xyz*shadowValue,texcolour.a*vColour.a);
  }else if(isPoints > 0) {
    outcolor = texture(diffuseTexture,gl_PointCoord) * MaterialDiffuse * vColour;
  } else {
    outcolor = vec4(((diffuse + specular + ambient).xyz*shadowValue),MaterialDiffuse.a*vColour.a);
  }
  if(isEnvMapped > 0) {
    reflected = reflect(-EE,NN);
    reflected = vec3(inverse(ViewMatrix) * vec4(reflected,0.0));
    outcolor += vec4(texture(envMap,reflected).xyz * envMapWeight * pf,outcolor.a);
  }

  if(isProjectionTextured > 0) {
    //tmp = normalize(NN * vec3(1.0,1.0,10.0));
    //reflected = reflect(-EE,tmp); //tmp);
    //reflected = vec3(inverse(ViewMatrix) * vec4(reflected,0.0));
    //outcolor += vec4(texture(projectionTexture,reflected.xy).xyz * projectionTextureWeight, outcolor.a);
    //UVWCoordProjectionTexture
    tmp1 = UVWCoordProjectionTexture.xyz / UVWCoordProjectionTexture.w;
    tmp2 = vec3((tmp1.x*0.5) + 0.5,(tmp1.y*0.5) + 0.5,tmp1.z);
    outcolor += vec4(texture(projectionTexture,tmp2.xy).xyz * projectionTextureWeight, outcolor.a);
    }
  
  return outcolor;
}

void main()
{
  vec3 reflected;
  vec4 outcolour = vec4(0.0);
  vec4 projCoord;
  vec3 tmp1, tmp2;
  vec3 NN = normalize(N); // surface normal
  vec3 EE = normalize(E); // eye vector
  vec3 VV = normalize(V); // vertex 3d
  float attenuation, shadowValue;
  vec4 emissive = MaterialEmissive;
  if(emitVColour > 0) { emissive *= vColour; }
  projCoord = UVWCoordProjectionTexture / UVWCoordProjectionTexture.w;

  if(numLights > 0) { // light 1
    attenuation = calcAttenuation(0);
    shadowValue = calcShadow(0);
    outcolour += calcFrag(0,NN,EE,attenuation,shadowValue);
  }

  if(numLights > 1) { // light 2
    attenuation = calcAttenuation(1);
    shadowValue = calcShadow(1);
    outcolour += calcFrag(1,NN,EE,attenuation,shadowValue);
  }

  if(numLights > 2) { // light 3
    attenuation = calcAttenuation(2);
    shadowValue = calcShadow(2);
    outcolour += calcFrag(2,NN,EE,attenuation,shadowValue);
  }

  if(numLights > 3) { // light 4
    attenuation = calcAttenuation(3);
    shadowValue = calcShadow(3);
    outcolour += calcFrag(3,NN,EE,attenuation,shadowValue);
  }

  if(numLights > 4) { // light 5
    attenuation = calcAttenuation(4);
    shadowValue = calcShadow(4);
    outcolour += calcFrag(4,NN,EE,attenuation,shadowValue);
  }

  if(numLights < 1) { // NO LIGHTS!
    float dotE = max(0.0, dot(NN,EE));
    if(isTextured > 0) {
      outcolour = vec4(texture(diffuseTexture,UVWCoord.xy).xyz*vColour.xyz*dotE,MaterialDiffuse.a*vColour.a);
    }else if(isPoints > 0){
            outcolour = texture(diffuseTexture,gl_PointCoord) * vColour * MaterialDiffuse; // * dotE * vColour;
    }else{      
      outcolour = vec4(MaterialDiffuse.xyz*vColour.xyz*dotE,MaterialDiffuse.a*vColour.a);
    }
    if(isEnvMapped > 0) {
      reflected = reflect(-EE,NN);
      reflected = vec3(inverse(ViewMatrix) * vec4(reflected,0.0));
      outcolour += vec4(texture(envMap,reflected).xyz * envMapWeight, outcolour.a);      
      // outcolour += vec4(texture(envMap,reflected).xyz * 0.25,1.0);
    }    
  } else {
    outcolour /= float(numLights);
  }

  if((isProjectionTextured > 0) && (numLights < 1)) {
      tmp1 = projCoord.xyz; //projCoord.z; //vec3((projCoord.x * 2.0)+1.0,(projCoord.y * 2.0)+1.0,projCoord.z);    
      tmp2 = vec3((tmp1.x*0.5) + 0.5,(tmp1.y*0.5) + 0.5,tmp1.z);
      outcolour = mix(outcolour,vec4(texture(projectionTexture,tmp2.xy).xyz, outcolour.a),projectionTextureWeight);
  }
  
  xtmOutColour = vec4(outcolour.xyz+emissive.xyz,outcolour.a);
}

// end file
