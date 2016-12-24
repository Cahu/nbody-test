#version 330

in  vec3 iValue;
out vec3 oValue;

void main()
{
	oValue.x =  - iValue.x;
	oValue.y =  - iValue.y;
	oValue.z =  - iValue.z;
}
