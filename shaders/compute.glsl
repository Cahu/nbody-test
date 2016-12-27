#version 430


layout (local_size_x = 1024, local_size_y = 1) in;

layout (std140, binding = 0) buffer Pos {
    vec4 positions[];
};

layout (std140, binding = 1) buffer Vel {
    vec4 velocities[];
};


// gravitational constant
float bigG = 1;

// time step
float tickLen = 0.2;

vec3 gravityAccel(in uint other_idx)
{
    uint gid = gl_GlobalInvocationID.x;

	vec3 position       = positions[gid].xyz;
	vec3 other_position = positions[other_idx].xyz;
    vec3 diff_position  = other_position - position;

	float other_mass = positions[other_idx].w;
	float dist       = length(diff_position);

	vec3 acceleration = diff_position * bigG * other_mass / pow(dist*dist + 2000, 1.5);

	return acceleration;
}

void main()
{
    uint gid = gl_GlobalInvocationID.x;

	uint total_stars  = gl_WorkGroupSize.x * gl_NumWorkGroups.x;
	vec3 acceleration = vec3(0, 0, 0);

	for (uint i = 0; i < total_stars; i++)
	{
		if (i == gid)
			continue;

		acceleration += gravityAccel(i);
	}

    float mass     = positions[gid].w;
	vec3  position = positions[gid].xyz;
    vec3  velocity = velocities[gid].xyz;

    position += tickLen * velocity;
    velocity += tickLen * acceleration;

    barrier();

    positions[gid]  = vec4(position, mass);
    velocities[gid] = vec4(velocity, 0);
}
