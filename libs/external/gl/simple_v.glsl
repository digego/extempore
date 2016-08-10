// start file

#version 330

uniform mat4 ModelMatrix;
uniform mat4 ViewMatrix;
uniform mat4 ProjectionMatrix;
uniform mat3 NormalMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 ModelViewProjectionMatrix;

layout (location = 0) in vec4 xtmVertex;
layout (location = 1) in vec3 xtmNormal;
layout (location = 2) in vec3 xtmUVW;
layout (location = 3) in vec4 xtmColour;

void main() {
   gl_Position = ModelViewProjectionMatrix * xtmVertex;
}

// end file
