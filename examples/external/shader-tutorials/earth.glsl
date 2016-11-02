// Orbital Flight by Jerome Liard, August 2016
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
// https://www.shadertoy.com/view/4lVGRy

// Hold SPACE + LMB to look around.
// The camera switches every 30 seconds, there are currently 8 different paths.
// It is meant to be slow pace, but right arrow lets you fast forward.

// Some camera paths are kepler orbits, others are just lookats.
// Perf note: atmosphere sections of the shader could be way faster using buffers,
// but I just brute forced instead, except for the Ie term which is offline + manual curve fitting.
// It would be slow on older cards.

// The shader tries to make the most of shadertoy's mushroom texture using layering,
// tiling, bombing of vortex distortions and silly exaggerations to make clouds shine more in the penumbra zone.

#version 330

//#define FORCE_CAMERA 2.0 // force camera, int values in [0,CAMERA_NUM[
#define CAMERA_TIME_RESET // make camera predictable by resetting
#define CAMERA_PERIOD 30.0 // time we stay on each camera, in seconds
#define GLOBALTIME (iGlobalTime+0.0) // offset sets initial view
//#define GLOBALTIME (CAMERA_PERIOD*0.0+25.0)

#define CAMERA_NUM 8.0

// set camera order
#define CAMERA_SPECULAR_CLOSE   0.0
#define CAMERA_CLOUDS           1.0
#define CAMERA_TAKE_OFF_BLUE    2.0
#define CAMERA_ORBITING_FAR     3.0
#define CAMERA_SPECULAR_FAR     4.0
#define CAMERA_TAKE_OFF_SUNRISE 5.0
#define CAMERA_ORBITING_CLOSE   6.0
#define CAMERA_MOON_WIP         7.0 // moon has no surface shader yet...

#define CLOUD_FLOW
#define EARTH_ROTATION
//#define HD_BLACK_BANDS
//#define SUPER_SAMPLE_HORIZON
//#define NO_EXPOSE
//#define NO_MOUSE_CTRL

#define PI			3.141592654
#define FLT_MAX		1000000.0

#define RED			vec3(1,0,0)
#define GREEN		vec3(0,1,0)
#define BLUE		vec3(0,0,1)
#define WHITE		vec3(1,1,1)
#define BLACK		vec3(0,0,0)
#define MAGENTA		vec3(1,0,1)
#define YELLOW		vec3(1,1,0)
#define CYAN		vec3(0,1,1)
#define GREY50		vec3(0.5,0.5,0.5)

#define R01         vec2( 0.999847695, 0.017452406 )
#define R05         vec2( 0.996194698, 0.087155742 )
#define R30         vec2( 0.866025403, 0.5 )
#define R45         vec2( 0.707106781, 0.707106781 )
#define R60         R30.yx

#define const

// hash functions from David Hoskins's https://www.shadertoy.com/view/4djSRW

float hash11( float p )
{
	vec2 p2 = fract( vec2( p * 5.3983, p * 5.4427 ) );
	p2 += dot( p2.yx, p2.xy + vec2( 21.5351, 14.3137 ) );
	return fract( p2.x * p2.y * 95.4337 );
}

float hash12( vec2 p )
{
	p  = fract( p * vec2( 5.3983, 5.4427 ) );
	p += dot( p.yx, p.xy + vec2( 21.5351, 14.3137 ) );
	return fract( p.x * p.y * 95.4337 );
}

vec2 hash22( vec2 p )
{
	p  = fract( p * vec2( 5.3983, 5.4427 ) );
	p += dot( p.yx, p.xy +  vec2( 21.5351, 14.3137 ) );
	return fract( vec2( p.x * p.y * 95.4337, p.x * p.y * 97.597 ) );
}

vec3 hash32( vec2 p )
{
	p  = fract( p * vec2( 5.3983, 5.4427 ) );
	p += dot( p.yx, p.xy +  vec2( 21.5351, 14.3137 ) );
	return fract( vec3( p.x * p.y * 95.4337, p.x * p.y * 97.597, p.x * p.y * 93.8365 ) );
}

vec4 xyz1( vec3 v ) { return vec4( v, 1. ); }
vec4 xyz0( vec3 v ) { return vec4( v, 0. ); }
vec3 xy0( vec2 v ) { return vec3( v, 0. ); }
vec4 xy01( vec2 v ) { return vec4( v, 0., 1. ); }
float smoothstep_unchecked( float x ) { return ( x * x ) * ( 3.0 - x * 2.0 ); }
vec2 smoothstep_unchecked( vec2 x ) { return ( x * x ) * ( 3.0 - x * 2.0 ); }
vec3 smoothstep_unchecked( vec3 x ) { return ( x * x ) * ( 3.0 - x * 2.0 ); }
float smoothstep_unchecked_6( float x ) { return x * x * x * x * x * ( 6.0 - 5.0 * x ); }
float smoothbump( float c, float r, float x ) { return 1.0 - smoothstep_unchecked( min( abs( x - c ), r ) / r ); }
float remap( float x, float a, float b ) { return clamp( ( x - a ) / ( b - a ), 0., 1. ); }
float tri( float x, float spacing ) { return spacing - ( abs( ( spacing * fract( x / spacing ) - spacing * 0.5 ) ) + spacing * 0.5 ); }
float tri( float x ) { return 1.0 - abs( fract( x * 0.5 ) - 0.5 ) * 2.0; }
float saturate( float x ) { return clamp( x, 0.0, 1.0 ); }
vec2 perp( vec2 v ) { return vec2( -v.y, v.x ); }
float contrast( float x, float s ) { return ( x - 0.5 ) * s + 0.5; }
vec3 contrast( vec3 x, vec3 s ) { return ( x - 0.5 ) * s + 0.5; }
float lensqr( vec3 v ) { return dot( v, v ); }
float lensqr( vec2 v ) { return dot( v, v ); }
float pow2( float x ) { return x * x; }
float pow3( float x ) { return x * x * x; }
float pow4( float x ) { x *= x; x *= x; return x; }
vec4 pow2( vec4 x ) { return x * x; }
vec4 pow3( vec4 x ) { return x * x * x; }
vec4 pow4( vec4 x ) { x *= x; x *= x; return x; }
float soft_max( float x, float y, float k ) { return log( exp( k * x ) + exp( k * y ) ) / k; }
float soft_max( float x, float y, float z, float k ) { return log( exp( k * x ) + exp( k * y ) + exp( k * z ) ) / k; }
float powerful_scurve( float x, float p1, float p2 ) { return pow( 1.0 - pow( 1.0 - clamp( x, 0.0, 1.0 ), p2 ), p1 ); }

struct Ray
{
	vec3 o;
	vec3 d;
};

Ray get_view_ray( vec2 normalized_pos, float z, float aspect, float tan_half_fovy )
{
	Ray view_ray;
	view_ray.o = vec3( normalized_pos * vec2( aspect, 1.0 ) * tan_half_fovy, -1.0 ) * z;
	view_ray.d = normalize( view_ray.o );
	return view_ray;
}

mat4 lookat( vec3 eye, vec3 center, vec3 up )
{
	vec3 z = normalize( eye - center );
	vec3 x = normalize( cross( up, z ) );
	vec3 y = cross( z, x );
	return (mat4( vec4( x, 0.0 ), vec4( y, 0.0 ), vec4( z, 0.0 ), vec4( eye, 1.0 ) ));
}

vec2 unit_vector2( float angle ) { return vec2( cos( angle ), sin( angle ) ); }
vec2 rotate_with_unit_vector( vec2 p, vec2 cs ) { return vec2( cs.x * p.x - cs.y * p.y, cs.y * p.x + cs.x * p.y ); }
vec2 rotate_with_angle( vec2 p, float a_angle ) { return rotate_with_unit_vector( p, unit_vector2( a_angle ) ); }

// theta is angle with the z axis, range [0,pi].
// phi is angle with x vectors on z=0 plane, range [0,2pi].
vec3 zup_spherical_coords_to_vector( float theta, float phi )
{
	vec2 theta_vec = unit_vector2( theta );
	vec2 phi_vec = unit_vector2( phi );
	return vec3( theta_vec.y * phi_vec, theta_vec.x );
}

mat4 zup_spherical_lookat2( float theta, float phi )
{
	vec3 z = zup_spherical_coords_to_vector( theta, phi );
	vec3 x = zup_spherical_coords_to_vector( theta + PI * 0.5, phi );
	vec3 y = cross( z, x );
	return (mat4( vec4( x, 0.0 ), vec4( y, 0.0 ), vec4( z, 0.0 ), vec4( 0.0, 0.0, 0.0, 1.0 ) ));
}

vec3 yup_spherical_coords_to_vector( float theta, float phi )
{
	return zup_spherical_coords_to_vector( theta, phi ).yzx;
}

mat4 yup_spherical_offset( float theta, float phi )
{
	vec3 y = yup_spherical_coords_to_vector( theta, phi );
	vec3 z = yup_spherical_coords_to_vector( theta + PI * 0.5, phi );
	vec3 x = cross( y, z );
	return (mat4( vec4( x, 0.0 ), vec4( y, 0.0 ), vec4( z, 0.0 ), vec4( 0, 0, 0, 1 ) ));
}

mat4 z_rotation( float angle )
{
	vec2 v = unit_vector2( angle );
	return mat4( vec4( v.x, v.y, 0.0, 0.0 ), vec4( -v.y, v.x, 0.0, 0.0 ), vec4( 0, 0, 1, 0 ), vec4( 0, 0, 0, 1 ) );
}

// use with constants...
#define POW0(x) 1.0
#define POW1(x) (x)
#define POW2(x) (POW1(x)*(x))
#define POW3(x) (POW2(x)*(x))
#define POW4(x) (POW3(x)*(x))
#define POW5(x) (POW4(x)*(x))
#define POW6(x) (POW5(x)*(x))

// project this on line (O,d), d is assumed to be unit length
vec3 project_on_line1( vec3 P, vec3 O, vec3 d ) { return O + d * dot( P - O, d ); }

#define layered5_pass_scale(func,p,a)((func(p,POW0(2.0),a)*POW1(0.5)+func(p,POW1(2.0),a)*POW2(0.5)+func(p,POW2(2.0),a)*POW3(0.5)+func(p,POW3(2.0),a)*POW4(0.5)+func(p,POW4(2.0),a)*POW5(0.5))*(1.0/(1.0-POW5(0.5))))

struct NoiseTiledParams
{
	vec3 eye, n, p;
	float bias;
};

vec2 grid3( vec2 x, vec2 r ) { return smoothstep( r, vec2( 1.0 ), abs( ( fract( x ) - vec2( 0.5 ) ) * 2.0 ) ); }

float tile_tex( in vec2 uv, float s, NoiseTiledParams ntp )
{
	uv *= s;

	float bias = 0.0;

	float edge = 1.0 - dot( normalize( ntp.eye - ntp.p ), ntp.n );
	bias = -1.2 * edge; // bias the mipmap blur horror on edges

	bias += ntp.bias;

	// make the texture tilable
	float a0 = texture2D( iChannel1, vec2( uv.x - 0.0, uv.y - 0.0 ), bias ).x; // main image
	float b0 = texture2D( iChannel1, vec2( uv.x - 0.0, uv.y - 0.5 ), bias ).x; // fill seams
	// it's ok to fract on the seam filling lookups
	// without fract we are smooth tiled everywhere but at mipmap horror line
	float a1 = texture2D( iChannel1, vec2( fract( uv.x - 0.5 ), uv.y - 0.0 ), bias ).x; // fill seams
	float b1 = texture2D( iChannel1, vec2( fract( uv.x - 0.5 ), uv.y - 0.5 ), bias ).x; // fill seams

	float r = 1.0 - 0.2; // note: don't divide by s... 0,1 here
	uv = grid3( uv, vec2( r ) );
	return mix( mix( a0, b0, uv.y ), mix( a1, b1, uv.y ), uv.x );
}

float fbm5_tiled_clouds( vec2 p, NoiseTiledParams ntp ) { return layered5_pass_scale( tile_tex, p, ntp ); }

// just return the delta
vec3 vortex( vec2 q, float max_twist, float aa_scale )
{
	vec2 c = vec2( 0.5, 0.5 );
	float r0 = 0.5;
	vec2 v = ( q - c );
	float r = length( v );
	float x = min( r / r0, 1.0 );
	float aa = pow2( 1.0 - x );
	return vec3( c + rotate_with_angle( q - c, aa * max_twist ) - q, aa * aa_scale );
}

vec3 vortex_bombing( vec2 p, float scale, float max_twist, float aa_scale, float probability )
{
	p *= scale;
	vec2 pi = floor( p );
	vec2 pf = fract( p );
	vec3 x = vec3( 0.0, 0.0, 0.0 );
	for ( int i = -1; i <= 1; ++i )
	{
		for ( int j = -1; j <= 1; ++j )
		{
			vec2 o = vec2( float( i ), float( j ) );
			vec2 pj = pi + o;
			pj.x = mod( pj.x, scale ); // we are mapping a sphere so want same distortion at the u=0, u=1 limit
			vec3 rj = hash32( pj );
			if ( rj.z <= probability ) x += vortex( pf - o - rj.xy, max_twist * rj.z, aa_scale * rj.z ); //p - ( pj + rj.xy ) == ( pi + pf ) - ( pj + rj.xy ) == pf - o - rj.xy
		}
	}
	return x;
}

float calc_angle( vec2 v ) { return atan( v.y, v.x ); }

vec2 sphere_trace( Ray ray, float radius, vec3 C )
{
	vec3 O = ray.o;
	vec3 d = ray.d;
	float tp = dot( C - O, d ); // P = project C on line (O,d)
	vec3 P = O + d * tp;
	float h_sqr = lensqr( P - C );
	float radius_sqr = radius * radius;
	if ( h_sqr > radius_sqr ) return vec2( FLT_MAX, FLT_MAX ); // ray missed the sphere
//	bool start_inside = lensqr( O - C ) <= radius_sqr; // start inside the sphere?
	float dt = sqrt( radius_sqr - h_sqr ); // distance from P to In (near hit) and If (far hit)
//	if ( start_inside )	return vec2(FLT_MAX,tp+dt);	// order In→O→If // record only far hit If
//	if ( tp < 0.0 )	return vec2(FLT_MAX,FLT_MAX); // order In→If→O // O is outside the sphere and beyhond If, no hit
	return vec2( tp - dt, tp + dt ); // record 2 hits In, If
}

#define earth_center vec3(0.0,0.0,0.0)
//const float atm_scale=1e+3f; // 1=1m
#define atm_scale 1.0 // 1=1km
#define sun_direction vec3(0.0,1.0,0.0) // normalized please
const float earth_angular_velocity = ( 2.0 * PI / ( 24.0 * 60.0 * 60.0 ) );
const float earth_radius = 6378.15 * atm_scale;
const float sun_radius = 6.955e+5 * atm_scale; // for render only
#define sun_dist (1.49e+8*atm_scale) // for render only
const float sun_cos = 0.999989; // for render only
const float sun_solid_angle = 0.0093355;  // sun view angle
const vec3 sun_center = earth_center + sun_direction * sun_dist;
//const vec3 moon_direction=sun_direction; // full sun eclipse
#define moon_direction /*normalize(*/vec3(1.0,0.0,0.0)/*)*/	// moon close to sun
//const vec3 moon_direction=normalize(vec3(0.01f,1.0,0.0)); // moon close to sun
//const vec3 moon_direction=normalize(vec3(0.015f,1.0,-0.02f));	// moon close to sun, a bit far appart
//const vec3 moon_direction=normalize(vec3(0.0,-1.0,0.0)); // moon fully occluded
//const vec3 moon_direction=normalize(vec3(0.016f,-1.0,0.0)); // half moon
const float moon_radius = 1738.14 * atm_scale;
#define moon_dist (384400.0*atm_scale)
const float moon_cos = 0.99999;
const vec3 moon_center = earth_center + moon_direction * moon_dist;
const float H0_r = 0.7994 * atm_scale;
const float H0_m = 1.2 * atm_scale;
const float atm_max = 20.0 * atm_scale; // as small as possible, large enough to accomodate H0_ values
const vec3 beta_r = ( 1.0 / atm_scale ) * vec3( 1.30126e-2, 3.04053e-2, 7.42317e-2 ); // calculate this offline
//const vec3 beta_r=atm_scale*vec3(1.1588e-2,3.42165e-2,5.55848e-2); // calculate this offline
const vec3 beta_m = ( 1.0 / atm_scale ) * vec3( 1.30126e-2, 3.04053e-2, 7.42317e-2 );
//const float g=-0.8f;
const vec3 Is = vec3( 1.0, 1.0, 1.0 ) * 35.0;
const vec3 earth_diffuse_reflection = vec3( 0.2 );
const float cloud_start = 1.0 * atm_scale;
const float cloud_end = 10.0 * atm_scale;
//const float ie_cost_min=-0.0871;//95
//const float ie_cost_min=-0.1736;//100deg
const float ie_cost_min = -0.3420; //110deg

float sunh( float viewdist )
{
	return viewdist * ( sun_radius / sun_dist );
//	return viewdist * tan( sun_solid_angle * 0.5 );
}

// params for Ie term manual fit
struct IeApproxS
{
	float a;
	float k;
	float sx;
	float sy;
	float last_bit;
};

IeApproxS ie_params_r;
IeApproxS ie_params_g;
IeApproxS ie_params_b;

vec2 calc_rho( float h ) { return exp( -vec2( h ) / vec2( H0_r, H0_m ) ); }

struct vec6
{
	vec3 r;
	vec3 m;
};

vec6 init_vec6( float s )
{
	vec6 val;
	val.r = vec3( s );
	val.m = vec3( s );
	return val;
}

// integration code is setup with trapezoidal rule, wanted adaptative step size (ended up not doing)
struct OpticalDepth
{
	Ray ray;
	vec6 ret;
	float l;
	vec6 f0;
	int i;
};

void OpticalDepth_start( inout OpticalDepth od, Ray a_ray, float tA )
{
	od.ray = a_ray;
	od.ret = init_vec6( 0.0 );
	od.l = tA;
	od.f0 = init_vec6( 0.0 );
	od.i = 0;
}

void OpticalDepth_step( inout OpticalDepth od, float dl, vec6 mul )
{
	vec3 p = od.ray.o + od.ray.d * od.l;
	vec2 rho = calc_rho( length( p - earth_center ) - earth_radius );
	vec6 f1;
	f1.r = rho.x * mul.r;
	f1.m = rho.y * mul.m;
	if ( od.i != 0 )
	{
		od.ret.r += ( f1.r + od.f0.r ) * dl;
		od.ret.m += ( f1.m + od.f0.m ) * dl;
	}
	od.l += dl;
	od.f0 = f1;
	od.i += 1;
}

vec6 OpticalDepth_current( in OpticalDepth od )
{
	vec6 ret = od.ret;
	ret.r *= beta_r * 0.5; // integration 0.5
	ret.m *= beta_m * 0.5; // integration 0.5
	return ret;
}

vec6 calc_optical_depth( Ray ray, float tA, float tB )
{
	OpticalDepth o;
	OpticalDepth_start( o, ray, tA );
#define NUM_SAMPLES_OD 20
	float dl = ( tB - tA ) / float( NUM_SAMPLES_OD - 1 );
	vec6 tmp = init_vec6( 1.0 );
	for ( int i = 0; i < NUM_SAMPLES_OD; ++i ) OpticalDepth_step( o, dl, tmp );
	return OpticalDepth_current( o );
}

// http://nishitalab.org/user/nis/cdrom/sig93_nis.pdf
float CornetteSingleScatteringPhaseFunction( float cos_theta, float g ) { float g2 = g * g; return 3.0 * ( 1.0 - g2 ) * ( 1.0 + pow2( cos_theta ) ) / ( 2.0 * ( 2.0 + g2 ) * pow( 1.0 + g2 - 2.0 * g * cos_theta, 1.5 ) ); }

// == CornetteSingleScatteringPhaseFunction( cos_theta, 0.0 )
float RayleighScattering( float cos_theta ) { return 0.75 * ( 1.0 + cos_theta * cos_theta ); }

// https://www.astro.umd.edu/~jph/HG_note.pdf HG, g in [-1,1]
float HenyeyGreensteinPhaseFunction( float cos_theta, float g ) { float g2 = g * g; return ( 1.0 / ( 4.0 * PI ) ) * ( 1.0 - g2 ) / pow( 1.0 + g2 - 2.0 * g * cos_theta, 1.5 ); }

float calc_Fr_m( float cos_theta, float g )
{
	return CornetteSingleScatteringPhaseFunction( cos_theta, g );
//	return HenyeyGreensteinPhaseFunction( cos_theta, g ) * PI * PI; // ??
}

float calc_Fr_r( float cos_theta )
{
	return RayleighScattering( cos_theta  );
}

bool in_earth_sun_shadow( vec3 P )
{
	return ( dot( P, sun_direction ) < 0.0 )
		   && ( lensqr( P - project_on_line1( P, earth_center, sun_direction ) ) < earth_radius * earth_radius );
}

bool in_moon_sun_shadow( vec3 P )
{
	return ( dot( P - moon_center, sun_direction ) < 0.0 )
		   && ( lensqr( P - project_on_line1( P, moon_center, sun_direction ) ) < moon_radius * moon_radius );
}

// this is a manual fit of the offline precalculated Ie 1d table for each r,g,b, plus slight tweaks
float ie_approx_S( float cos_alpha, IeApproxS params )
{
	float y =
		soft_max( cos_alpha * params.sx, params.a, params.k ) * params.sy
		- soft_max( 0.0, params.a, params.k ) * params.sy;

	float c = 0.98;
	if ( cos_alpha > c ) y *= ( 1.0 + params.last_bit * ( cos_alpha - c ) / ( 1.0 - c ) );
	return max( 0.0, y );
}

void Ie_ie_params_init()
{
	ie_params_r.a = 1.07143104;
	ie_params_r.k = 0.578571617;
	ie_params_r.sx = 12.857152938;
	ie_params_r.sy = 2.785715818;
	ie_params_r.last_bit = 0.042857192;

	ie_params_g.a = 5.250002384;
	ie_params_g.k = 0.385714441;
	ie_params_g.sx = 24.000009536;
	ie_params_g.sy = 1.357144474;
	ie_params_g.last_bit = 0.126428619;

	ie_params_b.a = 7.285716533;
	ie_params_b.k = 0.435714453;
	ie_params_b.sx = 13.285723686;
	ie_params_b.sy = 2.928573131;
	ie_params_b.last_bit = 0.300000011;
}

vec3 calc_Ie_shaderfunc( float cos_alpha )
{
	Ie_ie_params_init();
#if 0
	float ie_params_bleed = -0.15; // exaggerate Ie bleed a little bit
	cos_alpha = clamp( cos_alpha, ie_cost_min, 1.0 );
	cos_alpha = ie_params_bleed + ( cos_alpha - ie_params_bleed ) * ( 1.0 - ie_params_bleed );
#else
	float bleed = 0.02; //0.0 means no bleed, sunset cameras need enough bleeding to look interesting
	cos_alpha = 1.0 + ( cos_alpha - 1.0 ) * ( 1.0 - bleed ); // exaggerate Ie bleed a little bit
	cos_alpha = clamp( cos_alpha, ie_cost_min, 1.0 - 0.019 ); // cos alpha=1 has a weird blob, clamp a bit...
#endif
	return vec3( ie_approx_S( cos_alpha, ie_params_r ),
				 ie_approx_S( cos_alpha, ie_params_g ),
				 ie_approx_S( cos_alpha, ie_params_b ) );
}

struct LameTweaks
{
	float earth_rot_time;
	float cloud_flow_time;
	// no PBR no life
	float specular_hack;
	vec3 cloud_hack;
};

float cloudSphereMap( vec2 p, mat4 camera, vec3 n, float bias, LameTweaks lame_tweaks )
{
	vec2 p0 = p;

	float pole = 0.1;
	p.y = ( p.y - pole ) / ( 1.0 - 2.0 * pole );

	// p0 is in x 0,1
	// q0 is in x 0,2

	vec3 q = vec3( p * vec2( 2, 1 ), 0.0 );

	vec3 q0 = q;

//	q += vortex_bombing( q.xy,  1.0, 1.0, 1.0, 0.0 ) * POW0( 0.5 ); // 1
//	q += vortex_bombing( q.xy,  2.0, 1.0, 1.0, 0.0 ) * POW1( 0.5 ); // 2
//	q += vortex_bombing( q.xy,  4.0, 1.0, 1.0, 0.0 ) * POW2( 0.5 ); // 3
	q += vortex_bombing( q.xy,  8.0, 3.0, 1.0, 0.9 ) * POW3( 0.5 ); // 4
//	q += vortex_bombing( q.xy, 16.0, 3.0, 1.0, 1.0 ) * POW4( 0.5 ); // 5
	q += vortex_bombing( q.xy, 32.0, 2.7, 5.5, 0.85 ) * POW5( 0.5 ); // 6
//	q += vortex_bombing( q.xy, 64.0, 1.0, 1.0, 0.0 ) * POW6( 0.5 ); // 7

	vec2 qoff = vec2( 0.0, 0 );
#ifdef CLOUD_FLOW
	qoff.x = lame_tweaks.cloud_flow_time * earth_angular_velocity; //cloud flow (doesn't fix black line)
#endif

	NoiseTiledParams ntp;
	ntp.eye = camera[3].xyz;
	ntp.n = n;
	ntp.p = n * earth_radius;
	ntp.bias = bias;

	float a = fbm5_tiled_clouds( q.xy * 4.0 + qoff, ntp );

	a *= 1.0 - smoothstep( 0.5 - pole * 3.4, 0.5, abs( p0.y - 0.5 ) ); // would like to do better than that...

	float a0 = a;

	{
		//increase density on areas that have vortices
		a += length( q - q0 ) * 0.5;
		a += q.z * q.z * 5.0;
	}

	// add a little bit more oompf detail, helps overall + on cloud close ups
	a += a0 * fbm5_tiled_clouds( q.xy * 8.0 + qoff, ntp ) * 0.5;

	a = contrast( a + 0.05, 2.75 ); // higher contrast = deeper blue if we keep negative cloud
	a = soft_max( a, 0.0, 15.0 );
	return a;
}

float cloudMap( vec3 n, mat4 camera, float bias, LameTweaks lame_tweaks )
{
	vec3 n0 = n;
#ifdef EARTH_ROTATION
	n.xy = rotate_with_angle( n.xy, lame_tweaks.earth_rot_time * earth_angular_velocity );
#endif
	float theta = acos( n.z );
	float phi = calc_angle( n.xy ) + PI; // assume range 0,1

	return cloudSphereMap( vec2( phi * 0.5, theta ) * ( 1.0 / PI ), camera, n0, bias, lame_tweaks );
}

struct CloudOut
{
	vec3 sphere_point, sphere_normal;
	float cloud;
	bool hit;
};

// just one sphere lookup
CloudOut cloudTraceFlat( Ray ray, mat4 camera, float bias, LameTweaks lame_tweaks )
{
	CloudOut ret;
	ret.cloud = 0.0;
	ret.hit = false;
	float cloud_height = mix( cloud_start, cloud_end, 0.5 ); // cloud alt should be a number in 0-1
//	float cloud_height = mix( cloud_start, cloud_end, 10.0 ); // over the top shadow... fun
	vec2 tb = sphere_trace( ray, earth_radius, earth_center );
	vec2 tc = sphere_trace( ray, earth_radius + cloud_height, earth_center );
	if ( tc.x == FLT_MAX ) return ret; // no intersection with cloud sphere
	if ( tc.x < 0.0 && tb.x != FLT_MAX && tb.x >= 0.0 ) return ret;
	vec3 p = ray.o + ( tc.x < 0.0 ? tc.y : tc.x ) * ray.d;
	ret.sphere_point = p;
	ret.sphere_normal = normalize( p - earth_center );
	ret.cloud = cloudMap( ret.sphere_normal, camera, bias, lame_tweaks );
	ret.hit = true;
	return ret;
}

// get a bit of volume
CloudOut cloudTrace( Ray ray, mat4 camera, float bias, LameTweaks lame_tweaks )
{
	CloudOut ret;
	ret.cloud = 0.0;
	ret.hit = false;

	float hcs = mix( cloud_start, cloud_end, 0.55 );
	float hce = mix( cloud_start, cloud_end, 0.8 );
	vec2 ts = sphere_trace( ray, earth_radius + hcs, earth_center ); // start
	vec2 te = sphere_trace( ray, earth_radius + hce, earth_center ); // end
	if ( te.x == FLT_MAX ) return ret; // ray line doesn't intersect a (and therefore, b since b is inside a)
	if ( te.y <= 0.0 ) return ret; // ray line intersects a(atm) but behind us
	vec2 range;
	if ( ts.x == FLT_MAX )
	{
		// inside cloud altitude range, looking at upper cloud shell
		// ray line intersects a
		// ray line doesn't intersect b
		range.x = max( 0.0, te.x );
		range.y = te.y;
	}
	else
	{
		// ray line intersects a
		// ray line intersects b
		if ( te.x > 0.0 )
		{
			// hitting clouds from above
			// ray hitting a from outside
			range.x = te.x;
			range.y = ts.x;
		}
		else if ( ts.x > 0.0 )
		{
			// below lower cloud layer, looking at lower cloud layer
			range.x = 0.0;
			range.y = ts.x;
		}
		else
		{
			// between cloud layers, looking at upper cloud layer
			range.x = 0.0;
			range.y = te.y;
		}
	}

	float t = range.x;
	float dt = ( range.y - range.x ) * ( 1.0 / 8.0 );
	float min_dist = FLT_MAX;
	float hcdv = 1.0 / ( hce - hcs );

	for ( int i = 0; i < 8; ++i )
	{
		vec3 p = ray.o + t * ray.d;
		float hr = length( p - earth_center );
		float hp = hr - earth_radius;
		vec3 n = ( p - earth_center ) / hr;

		float c = cloudMap( n, camera, bias, lame_tweaks );
		float hc = hcs + ( hce - hcs ) * saturate( c );

		float dh = hp - hc;

		if ( abs( dh ) < 0.4 && t <= range.y )
		{
			ret.cloud = c;
			ret.sphere_normal = n;
			ret.hit = true;

			t += dt * 0.5; // note: don't break, keep homing
		}
		else t += dt;
	}

	return ret;
}

// fade shadow term based on distance
float shadow_falloff( vec3 pa, vec3 pb )
{
	vec3 d = ( pa - pb );
	return 1.0 / ( 1.0 + lensqr( d ) * 0.00005 );
}

// implementation of http://nishitalab.org/user/nis/cdrom/sig93_nis.pdf
// using buffer and  all symmetries we could cache luts and make this shader considerably faster,
// but I wanted a no buffers shader, so brute force.

struct AtmOut
{
	vec3 vod_attn; //view ray atm scattering
	vec3 earth_p;
	vec3 earth_n;
	bool earth_surface;
	vec3 Iv;
};

vec3 calc_Iv( Ray view_ray, inout AtmOut atm_out, mat4 camera, LameTweaks lame_tweaks )
{
	vec2 tb = sphere_trace( view_ray, earth_radius, earth_center );
	vec2 ta = sphere_trace( view_ray, earth_radius + atm_max, earth_center );

	atm_out.earth_surface = ( tb.x > 0.0 && tb.x != FLT_MAX );
	atm_out.vod_attn = vec3( 1.0 );
	atm_out.Iv = vec3( 0.0 );

//#define SPACECOLOR MAGENTAf // debug
#define SPACECOLOR BLACK

	if ( ta.x == FLT_MAX ) return SPACECOLOR; // view_ray line doesn't intersect a (and therefore, b since b is inside a)
	if ( ta.y <= 0.0 ) return SPACECOLOR; // return mix(SPACECOLOR,WHITE,0.7);	// view_ray line intersects a(atm) but behind us
	if ( tb.x <= 0.0 && tb.y >= 0.0 )	return GREEN; // inside (b)earth

	vec2 range;

	if ( tb.x == FLT_MAX )
	{
		// view_ray line intersects a
		// view_ray line doesn't intersect b
		range.x = max( 0.0, ta.x );
		range.y = ta.y;
	}
	else
	{
		// view_ray line intersects a
		// view_ray line intersects b
		if ( ta.x > 0.0 )
		{
			// ray hitting a from outside
			range.x = ta.x;
			range.y = tb.x;
		}
		else if ( tb.x > 0.0 )
		{
			range.x = 0.0;
			range.y = tb.x;
		}
		else
		{
			range.x = 0.0;
			range.y = ta.y;
		}
	}

	Ray sun_ray;
	sun_ray.d = sun_direction;

	float tP = range.x;

	OpticalDepth vod1;
	OpticalDepth_start( vod1, view_ray, tP ); // view ray optical depth

	OpticalDepth vod2;
	OpticalDepth_start( vod2, view_ray, tP ); // view ray optical depth

#define NUM_SAMPLES_IS 50 // we have to bump this number quite a bit to get decent integration

	float dl = ( range.y - range.x ) / float( NUM_SAMPLES_IS - 1 );
	if ( dl < 0.0 )	return YELLOW;

	for ( int i = 0; i < NUM_SAMPLES_IS; ++i )
	{
		float tP = vod2.l;

		vec3 P = view_ray.o + view_ray.d * tP;
		P = earth_center + normalize( P ) * max( earth_radius * 1.00001, length( P ) ); // make sure we don't start inside the earth when P is a hit point
		sun_ray.o = P;

		vec2 ta_sun = sphere_trace( sun_ray, earth_radius + atm_max, earth_center );
#if 0
		vec2 tb_sun=sphere_trace(sun_ray,earth_radius);
		bool P_in_earth_shadow=(tb_sun.x>0.0&&tb_sun.x!=FLT_MAX);
#else
		bool P_in_earth_shadow = in_earth_sun_shadow( P );
#endif
		vec6 tmp = init_vec6( 0.0 );

		if ( !P_in_earth_shadow )
//		if ( !in_moon_sun_shadow(P) )
		{
			vec6 tPPc = calc_optical_depth( sun_ray, 0.0/*P*/, ta_sun.y/*Pc*/ );    // note: ta_sun.y > 0.0

///			vec6 tPPa=calc_optical_depth(view_ray,range.x/*Pa*/,tP/*P*/); // don't recalculate the full tP,Pa path everytime...
			vec6 tPPa = OpticalDepth_current( vod1 );

			tmp.r = exp( -tPPc.r - tPPa.r );
			tmp.m = exp( -tPPc.m - tPPa.m );
		}

		OpticalDepth_step( vod1, dl, init_vec6( 1.0 ) );

		OpticalDepth_step( vod2, dl, tmp );
	}

	float cos_theta = dot( sun_direction, view_ray.d );

	vec3 Iv = Is *
		( ( OpticalDepth_current( vod2 ).r / ( 4.0 * PI ) ) * calc_Fr_r( cos_theta )
		  + ( OpticalDepth_current( vod2 ).m / ( 4.0 * PI ) ) * calc_Fr_m( cos_theta, 0.8 ) );

	atm_out.Iv = Iv;

	vec3 Ie = vec3( 0, 0, 0 );
	vec6 tPaPb = init_vec6( 0.0 );

	float cloud_shadow = 0.0;
	float specular = 0.0;

	if ( atm_out.earth_surface )
	{
		//return RED; // check earth pixel
		vec3 P = view_ray.o + view_ray.d * tb.x;
		vec3 n = normalize( P - earth_center );
		float cos_alpha = dot( sun_direction, n );
		Ie = calc_Ie_shaderfunc( cos_alpha * 1.0 );
		tPaPb = calc_optical_depth( view_ray, range.x/*Pa*/, tb.x/*Pb*/ );

		atm_out.earth_p = P;
		atm_out.earth_n = n;

		Ray cloud_shadow_ray;
		cloud_shadow_ray.d = sun_direction;
		cloud_shadow_ray.o = P + n * max( 1.00001, cloud_start * 0.5 ); //anything smaller than cloud_start and greater than a separation epsilon

		vec3 tangent = cross( sun_direction, n );
		vec3 np = normalize( cross( tangent, sun_direction ) );

//		vec2 acs = unit_vector2( radians( 5.0 ) );
		vec2 acs = R01;
		float w1 = 1.0;
		float w2 = 0.5;

		CloudOut cs;

		cloud_shadow_ray.d = sun_direction;
		cs = cloudTraceFlat( cloud_shadow_ray, camera, 0.0, lame_tweaks );
		cloud_shadow += cs.cloud * w1 * shadow_falloff( cloud_shadow_ray.o, cs.sphere_point );

		cloud_shadow_ray.d = sun_direction * acs.x + np * acs.y;
		cs = cloudTraceFlat( cloud_shadow_ray, camera, 0.0, lame_tweaks );
		cloud_shadow += cs.cloud * w2 * shadow_falloff( cloud_shadow_ray.o, cs.sphere_point );

		cloud_shadow_ray.d = sun_direction * acs.x - np * acs.y;
		cs = cloudTraceFlat( cloud_shadow_ray, camera, 0.0, lame_tweaks );
		cloud_shadow += cs.cloud * w2 * shadow_falloff( cloud_shadow_ray.o, cs.sphere_point );

		cloud_shadow *= ( 1.0 / ( w1 + w2 * 2.0 ) );

		vec3 l = sun_direction;
		vec3 e = -view_ray.d;
		vec3 h = normalize( e + l );
		float dp = dot( n, l );

		float specular_power = 75.0;

		specular = pow( max( dot( n, h ), 0.0 ), specular_power )
			* ( ( specular_power + 8.0 ) / ( 8.0 * PI ) )
			* max( 0.0, dp );
	}

	CloudOut co = cloudTrace( view_ray, camera, 0.0, lame_tweaks );
	float cloud = co.cloud;

	atm_out.vod_attn = exp( -OpticalDepth_current( vod1 ).r ); // for sun attn

	float dp = dot( co.sphere_normal, sun_direction );

	float s = ( 1.0 - saturate( cloud_shadow * ( 1.0 - cloud ) ) );

//	return vec3( ( 1.0 - s ) * 3.0, cloud, 0.0 );

	float earth_diffuse = 0.008;  // controls blue depth

	return ( 0.0
			 + earth_diffuse * s
			 + specular * ( 1.0 - saturate( cloud ) ) * lame_tweaks.specular_hack * s * s
			 + cloud
			 * // this add specks of gold to the clouds in the penumbra zone
			 ( 1.0
			   + smoothstep( -0.02, 0.012, dp )
			   * exp( ( cloud - cloud_shadow ) * lame_tweaks.cloud_hack.x )
			   * lame_tweaks.cloud_hack.y ) * lame_tweaks.cloud_hack.z

			) * Ie * exp( -tPaPb.r - tPaPb.m )

		   + Iv * ( 2.4 - ( 1.0 - s ) * 0.7 );
}

// linearly remap nl, cut is the value of nl that maps to 0
float warp_nl( float nl, float cut ) { return max( 0.0, ( nl - cut ) ) / ( 1.0 - cut ); }

float noise1s( in float x )
{
	x -= 0.5;

	float x0 = floor( x );
	float y0 = hash11( x0 );
	float y1 = hash11( x0 + 1.0 );

	return mix( y0, y1, smoothstep_unchecked( x - x0 ) );
}

float calcFallOff( float sd, float r, float p1, float p2 )
{
	float sd_last = sun_cos - r; //length of rays
	float g = 1.0 - saturate( max( sun_cos - sd, 0.0 ) / ( sun_cos - sd_last ) );
	return powerful_scurve( g, p1, p2 );
}

vec3 earthShader( Ray view_ray, mat4 camera, LameTweaks lame_tweaks, float exposure )
{
	AtmOut atm_out;

	vec3 ret = calc_Iv( view_ray, atm_out, camera, lame_tweaks );

	vec3 sun_color = atm_out.vod_attn * vec3( 1.0, 0.85, 0.71 );
	float sun_intensity = 0.0;

	float sd = dot( view_ray.d, sun_direction ); // assumes sun very far

	bool long_sun_flare = true;
	bool thin_flares = true;
	bool sun_glare = true;
	bool earth_clamped_sun_glare = true;

	if ( !atm_out.earth_surface )
	{
		vec2 tm = sphere_trace( view_ray, moon_radius, moon_center );
		if ( tm.x != FLT_MAX && tm.x > 0.0 )
		{
			vec3 moon_P = view_ray.o + view_ray.d * tm.x;
			float moon_sun_shadow = warp_nl( dot( normalize( moon_P - moon_center ), sun_direction ), -0.01 );
			float earth_sun_shadow = in_earth_sun_shadow( moon_P ) ? 0.0 : 1.0; // fixme: don't want sharp... precalc or fit something

			ret += vec3( 0.04 );
			ret += min( pow( moon_sun_shadow, 0.57 ), earth_sun_shadow ) * atm_out.vod_attn * 2.0;
		}
		else
		{
			if ( earth_clamped_sun_glare )
			{
				float gs = calcFallOff( sd, 0.01, 1.4, 0.05 );
				sun_intensity += 16.0 * gs;
			}
		}
	}

	float theta = acos( sd );
	float phi = calc_angle( view_ray.d.zx );
	vec2 phi_vec = unit_vector2( phi );

	float sun_visibility = 0.0;
	vec3 ep_sv = view_ray.o + sun_direction * dot( earth_center - view_ray.o, sun_direction );
	{
		float sh = sunh( length( ep_sv - view_ray.o ) );
		float h = length( ep_sv - earth_center );
		sun_visibility = saturate( smoothstep( -sh, sh, h - earth_radius ) );
//		float darkness = saturate( smoothstep( 0, earth_radius - sh, h ) );
//		if ( sun_visibility != 0.0 ) ret += RED;
	}

	float xsum = 0.0;

	if ( long_sun_flare )
	{
		float gs = calcFallOff( sd, 1e-3 * 12., 1.2, 0.07 );

		float spacing = 2.0 * PI / 6.0;
		for ( int i = 0; i < 6; ++i )
		{
			vec2 v = unit_vector2( float( i ) * spacing + 0.4 );
			float vv = abs( dot( phi_vec * theta, v ) );
			vv /= PI;
			vv *= 0.4;
			xsum += exp( -vv * 1500.0 ) * gs * 0.8;
		}
	}

	if ( thin_flares )
	{
		float gs = calcFallOff( sd, 1e-5 * 4.0, 1.4, 0.3 );

		// thin flares
		for ( int i = 0; i < 5; ++i )
		{
			float fi = float( i );
			float x = noise1s( phi * 3.5 + fi * 3.0 + 0.5 * sin( GLOBALTIME + fi ) );
			x = pow4( x );
			x = max( 0.0, contrast( x, 1.1 ) );
			x = gs * mix( 0.0, 1.0, x );
			xsum += x * 2.0 * gs;
		}
	}

	ret += xsum * ( atm_out.earth_surface ? 0.0 : 1.0 ) * pow( sun_visibility, 0.15 );

	if ( sun_glare )
	{
		// diffuse fall off
		sun_intensity += exp( -theta / PI * ( atm_out.earth_surface ? 70.0 : 27.0 ) ) * pow( sun_visibility, 0.20 );
	}

	ret += sun_intensity * sun_color;

//	float scene_luminance = 0.0;
//	scene_luminance = max( 0.0, dot( view_ray.d, sun_direction ) );

#ifndef NO_EXPOSE
//	ret = 1.0 - exp( -mix( 0.85, 0.05, scene_luminance/**sun_visibility*/ ) * ret );
	ret = 1.0 - exp( -exposure * ret );
#endif
	return ret;
}

// https://en.wikipedia.org/wiki/Kepler%27s_equation
float kepler_eq_solve( float M, float e/*,float& err*/ )
{
	float En = M;
	if ( e > 0.8 ) En = PI;
	for ( int i = 0; i < 3; ++i ) En = En - ( En - e * sin( En ) - M ) / ( 1.0 - e * cos( En ) );
//	err = max( fabsf( ( En - e * sin( En ) - M ) ), err );
	return En;
}

// n = mean motion n https://en.wikipedia.org/wiki/Mean_motion (consider this as the "angular speed")
// n = 2pi/period
// p,e see https://en.wikipedia.org/wiki/Kepler%27s_laws_of_planetary_motion
// p is the scale of the trajectory
// e the eccentricity, 0 for circle
// becomes ellipsoid (and smaller, so compensate by increasing p) as we get closer to 1
// above 0.8 kepler_eq_solve needs more iterations
// rmin=p/(1+e) closest distance to focus...perihelion
// rmax=p/(1-e) farthest distance to focus..aphelion
vec2 kepler_orbit( float t, float p, float e, float n/*, float& err*/ )
{
	float M = n * t;
	float E = kepler_eq_solve( M, e/*,err*/ );
	float a = p / ( 1.0 - e * e );
	float b = p / sqrt( 1.0 - e * e );
	float x = a * ( cos( E ) - e );
	float y = b * sin( E );
	return vec2( x, y );
}

// calculate p given rmin
float kepler_orbit_calc_p( float rmin, float e )
{
	return rmin * ( 1.0 + e );
}

struct KeplerOrbit
{
	float rmin; // min radius
	float period;
	float e; // ellipse eccentricity
};

struct KeplerOrbitRetval
{
	vec3 orbit_position;
	vec3 orbit_plane_normal;
};

KeplerOrbitRetval get_earth_camera_path_kepler( float t, in KeplerOrbit ko )
{
	KeplerOrbitRetval ret;
	// you can can play with highly elliptical orbits here, see
	// http://www.polaris.iastate.edu/EveningStar/Unit4/unit4_sub3.htm
	float n = 2.0 * PI / ko.period;
	vec2 p = kepler_orbit( t, kepler_orbit_calc_p( ko.rmin, ko.e ), ko.e, n );
	p = perp( p ); // start on y, where the sun is, symmetry more convenient to tweak orbit period
#if 1
	// define trajectory plane here (theta must be non zero if you want an inclination)
//	mat4 rep = zup_spherical_lookat2( radians( 90.0 ), radians( 90.0 ) ); // circle around penumbra zone
	mat4 rep = zup_spherical_lookat2( radians( 0.0 ), radians( 0.0 ) ); // trajectory inclination
	ret.orbit_plane_normal = rep[2].xyz;
	ret.orbit_position = ( rep * xy01( p ) ).xyz;
#else
	ret.orbit_plane_normal = vec3( 0, 0, 1 );
	ret.orbit_position = xy0( xx );
#endif
	return ret;
}

vec3 get_earth_camera_path_iss( float t, inout vec3 up )
{
	// needs high fov values
	float h = 340.0; //ISS
	float er = earth_radius / atm_scale;
	float dtheta = t * ( 27.6e+3 / 3600.0 ) / ( er + h ); //27.6km/h
	vec2 xx = unit_vector2( dtheta ) * ( er + h );
	mat4 rep = zup_spherical_lookat2( radians( 0.0 ), radians( 0.0 ) ); // trajectory inclination
	up = rep[2].xyz;
	return ( rep * xy01( xx ) ).xyz;
}

struct TangentView
{
	vec3 target_vector; // sphere center -> target unit vector
	vec3 tangent_disk_center;
	float tangent_disk_radius;
};

TangentView get_tangent_view( vec3 p, vec3 c, float r )
{
	TangentView ret;
	ret.target_vector = p - c;
	float dt = length( ret.target_vector );
	ret.target_vector /= dt;
	float e = ( r * r ) / dt;
	ret.tangent_disk_radius = sqrt( r * r - e * e );
	ret.tangent_disk_center = c + ret.target_vector * e;
	return ret;
}

// set view center on tangent of a sphere c,r
vec3 get_tangent_point( vec3 p, vec3 c, float r, vec3 up )
{
	TangentView tv = get_tangent_view( p, c, r );
	return tv.tangent_disk_center +
		   tv.tangent_disk_radius * normalize( cross( tv.target_vector, cross( up, tv.target_vector ) ) );
}

// reflect is builtin so use different name
float myreflect( float x, float a ) { return a + ( a - x ); }

// bounce time to make sure we are always travelling above the lit face of the earth
float bounce_time( float t, float period, inout float fade )
{
	t = mod( t, period );
	if ( t > period * 0.5 ) t -= period; // want mirrored time values
	float r = period * 0.005;
	float a = period * 0.25;
	fade *= 1.0 - smoothbump( a + r, r, clamp( abs( t ), a, a + r + r ) );
	if ( abs( t ) > ( a + r ) ) t = myreflect( t, ( ( t < 0.0 ) ? -1.0 : 1.0 ) * period * 0.25 );
	return t;
}

vec3 rotate_around( vec3 c, vec3 z, vec3 p, float angle )
{
	vec3 v = p - c;
	vec3 pp = project_on_line1( p, c, z );
	vec3 x = normalize( p - pp );
	vec3 y = cross( z, x );
	vec2 v2 = vec2( dot( v, x ), dot( v, y ) );
	v2 = rotate_with_angle( v2, angle );
	return pp + v2.x * x + v2.y * y;
}

// can't find the source shadertoy for those functions...
#define KEY_SPACE 32.
#define KEY_RIGHT 39.
bool key_toggle( float ascii ) { return (texture2D( iChannel0, vec2( ( ascii + .5 ) / 256., 0.75 ) ).x > 0.); }
bool key_state( float ascii ) { return (texture2D( iChannel0, vec2( ( ascii + .5 ) / 256., 0.25 ) ).x > 0.); }

mat4 get_earth_camera( inout float tan_half_fovy
					   , inout float fade
					   , float time
					   , out LameTweaks lame_tweaks
					   , inout float exposure )
{
	vec3 eye = vec3( 0, 0, 2.0 );
	vec3 center = vec3( 0, 0, 0 );

	float roll = 0.0;

#define IS_NEXT_INDEX(nn) ( abs( camera_index - nn ) < 0.01 )

#ifdef FORCE_CAMERA
	float camera_index = FORCE_CAMERA;
#else
	float camera_index = mod( floor( time / CAMERA_PERIOD ), CAMERA_NUM );
#endif
	fade = pow2( saturate( tri( time, CAMERA_PERIOD ) ) );

	float camera_time = mod( time, CAMERA_PERIOD );

	float earth_rot_time_scale = 1.0;
	float earth_rot_time_offset = 0.0;
	float cloud_flow_time_scale = 1.0;
	float cloud_flow_time_offset = 0.0;
	lame_tweaks.specular_hack = 0.25;
	lame_tweaks.cloud_hack = vec3( 2.0, 0.12, 0.5 );

#ifdef CAMERA_TIME_RESET
	time = camera_time;
#endif

	float mouse_ctrl = 1.0;
	vec2 mm_offset = vec2( 0.0, 0.0 );

	float x = camera_time * ( 1.0 / CAMERA_PERIOD );
	float xs6 = smoothstep_unchecked_6( x );

	if ( IS_NEXT_INDEX( CAMERA_ORBITING_CLOSE ) )
	{
		KeplerOrbit ko;
		ko.rmin = earth_radius + atm_max * 100.0;
		ko.period = 60.0 * 180.0;
		ko.e = 0.0024;
		float t = time * 20.0;
#ifdef CAMERA_TIME_RESET
		t = bounce_time( t, ko.period, fade );
#endif
		// time = 0; // eye should be on y=0,1,0 at time=0
		KeplerOrbitRetval kr = get_earth_camera_path_kepler( t, ko );
		eye = kr.orbit_position;
		vec3 up = normalize( cross( kr.orbit_plane_normal, eye ) );
		center = get_tangent_point( eye, earth_center, earth_radius * 0.98, up );
		tan_half_fovy = 0.09;
		roll = time * 0.02;
		earth_rot_time_scale = 1.0;
		earth_rot_time_offset = 2000.0;
		cloud_flow_time_scale = 1.0;
		cloud_flow_time_offset = 0.0;
		mouse_ctrl = 1.5;
		mm_offset = vec2( 0.0, 0.0 );
//		exposure = 0.45;
		lame_tweaks.cloud_hack.z = 0.35;
	}
	else if ( IS_NEXT_INDEX( CAMERA_ORBITING_FAR ) )
	{
		vec3 up = vec3( 0, 0,  1 );
		KeplerOrbit ko;
		ko.rmin = earth_radius + atm_max * 100.0;
		ko.period = 60.0 * 25.0;
		ko.e = 0.0024;
		float t = time * 10.0;
#ifdef CAMERA_TIME_RESET
		t = bounce_time( t, ko.period, fade );
#endif
		KeplerOrbitRetval kr = get_earth_camera_path_kepler( t, ko );
		eye = kr.orbit_position;
		up = normalize( cross( kr.orbit_plane_normal, eye ) );
		center = get_tangent_point( eye, earth_center, earth_radius * 0.965, up ); // don't look at real horizon, show more earth
		tan_half_fovy = 0.3; // 0.3 for sphere
//		roll = time*0.02;
		roll = PI * 3.0 / 4.0;
		earth_rot_time_scale = 12.0;
		earth_rot_time_offset = 0.0;
		cloud_flow_time_scale = 12.0;
		cloud_flow_time_offset = -10.0;
		mouse_ctrl = 0.6;
		mm_offset = vec2( 0.37, -0.08 );
		exposure = 0.75;
		lame_tweaks.specular_hack = 0.07;
		lame_tweaks.cloud_hack = vec3( 4.2, 0.12, 0.27 ); // too much decal
	}
	else if ( IS_NEXT_INDEX( CAMERA_TAKE_OFF_BLUE ) )
	{
		float cam_scale = 1e+3 * atm_scale;
		eye = vec3( 5.382552146, 3.343272924, -0.757502257 ) * cam_scale;
		vec3 n = normalize( eye );
		vec3 up = sun_direction;
		eye += n * 500.0 * xs6;
		center = get_tangent_point( eye, earth_center, earth_radius * mix( 1.0, 0.96, xs6 ), up ); // don't look at real horizon, show more earth
		tan_half_fovy = mix( 0.2, 0.09, xs6 );
		roll = PI * mix( 0.25, -0.02, 1.0 - exp( -x * 2.0 ) );
		earth_rot_time_scale = 198.0;
		earth_rot_time_offset = 39.9;
		cloud_flow_time_scale = 1.0;
		cloud_flow_time_offset = 0.0;
		lame_tweaks.specular_hack = 0.25;
		lame_tweaks.cloud_hack = vec3( 2.0, 0.12, 0.3 );
	}
	else if ( IS_NEXT_INDEX( CAMERA_CLOUDS ) )
	{
		float cam_scale = 1e+3 * atm_scale;
		eye = vec3( -5.704154491, -0.459553778, -3.58820796 ) * cam_scale;
		center = vec3( -5.828063488, -0.007100194, -2.705070018 ) * cam_scale;
		center = eye + ( center - eye ) * mix( 0.8, 0.8, x );
		eye = rotate_around( center, normalize( center ), eye, x * PI * 0.12 );
		tan_half_fovy = 0.05;
		roll = mix( -0.2, -0.3, x ) * PI;
		earth_rot_time_scale = 1.0;
		earth_rot_time_offset = -1600.0;
		cloud_flow_time_scale = 1.0;
		cloud_flow_time_offset = -900.0;
		mouse_ctrl = 2.5;
		exposure = 0.8;
		lame_tweaks.cloud_hack = vec3( 4.0, 0.12, 0.6 );
	}
	else if ( IS_NEXT_INDEX( CAMERA_SPECULAR_FAR ) )
	{
		float cam_scale = 1e+3 * atm_scale;
		eye = vec3( 5.421999931, -0.140298634, -3.804290771 ) * cam_scale;
		center = vec3( 5.581956386, 0.285743594, -2.913840293 ) * cam_scale;
		float rt = 0.25;
		eye.xz = rotate_with_angle( eye.xz, PI * rt );
		center.xz = rotate_with_angle( center.xz, PI * rt );
		tan_half_fovy = 0.62;
		earth_rot_time_scale = 60.0;
		earth_rot_time_offset = 20.0;
		cloud_flow_time_scale = 10.0;
		cloud_flow_time_offset = 100.0;
		mouse_ctrl = 0.26;
		mm_offset = vec2( -0.5, -0.29 );
		lame_tweaks.specular_hack = 150.0;
		lame_tweaks.cloud_hack = vec3( 4.5, 0.15, 1.0 );
	}
	else if ( IS_NEXT_INDEX( CAMERA_SPECULAR_CLOSE ) )
	{
		float cam_scale = 1e+3 * atm_scale;
		eye = vec3( 5.421999931, -0.140298634, -3.804290771 ) * cam_scale;
		center = vec3( 5.581956386, 0.285743594, -2.913840293 ) * cam_scale;
		float r = 0.0603;
		eye.xz = rotate_with_angle( eye.xz, PI * r );
		center.xz = rotate_with_angle( center.xz, PI * r );
		tan_half_fovy = 0.3;
		earth_rot_time_scale = 60.0;
		earth_rot_time_offset = 22.0;
		cloud_flow_time_scale = 10.0;
		cloud_flow_time_offset = 0.0;
		mouse_ctrl = 0.9;
		mm_offset = vec2( -0.84, -0.28 );
		lame_tweaks.specular_hack = 100.0;
		lame_tweaks.cloud_hack = vec3( 3.4, 0.15, 0.9 );
		exposure = 0.5;
	}
	else if ( IS_NEXT_INDEX( CAMERA_TAKE_OFF_SUNRISE ) )
	{
		float at1 = exp( -x * 10.0 ); // anim time 1: approach
		float at2 = smoothstep_unchecked_6( remap( x, 0.3, 1.0 ) ); // anim time 2: ascension
		float r = earth_radius + cloud_start * 5.0 + atm_max * at2 * 200.0;
		vec3 up = vec3( 0, 0, 0 ); up.zx = unit_vector2( -PI * 0.56 );
		vec3 sun_pos = sun_dist * sun_direction;
		vec3 tangent_point = get_tangent_point( sun_pos, earth_center, r, up );
		center = tangent_point;
		vec3 rail = normalize( sun_pos - tangent_point );
		float d0 = earth_radius * 0.08; // distance to tangent point is d
		float d1 = earth_radius * 0.03;
		eye = tangent_point + rail * mix( -d0, d1, x ); //
		vec3 end_center = get_tangent_point( eye, earth_center, earth_radius * 0.984, sun_direction );
		center = eye + rail;
		center = mix( center, end_center, at2 );
		tan_half_fovy = 0.15;
		earth_rot_time_scale = 1.0;
		earth_rot_time_offset = 0.0;
		cloud_flow_time_scale = 10.0;
		cloud_flow_time_offset = 90.0;
		exposure = mix( 0.4, 0.75, xs6 );
		lame_tweaks.cloud_hack = vec3( 3.0, 0.12, 0.65 );
		mm_offset = vec2( 0.0, -0.1 * xs6 );
		mouse_ctrl = 1.2;
		roll = PI * 0.0;
		lame_tweaks.specular_hack = 0.1;
	}
	else if ( IS_NEXT_INDEX( CAMERA_MOON_WIP ) )
	{
		float d = earth_radius * 0.5; // distance to tangent point is d
		float r = earth_radius + 4.0 * atm_max;
		vec3 moon_pos = moon_dist * moon_direction * 1.0;
		vec3 tangent_point = get_tangent_point( moon_pos, earth_center, r, vec3( 0, 1, 0 ) );
		center = tangent_point;
		eye = tangent_point + d * normalize( tangent_point - moon_pos ); //
		tan_half_fovy = 0.045;
		earth_rot_time_scale = 30.0;
		earth_rot_time_offset = 1700.0;
		cloud_flow_time_scale = 3.0;
		cloud_flow_time_offset = 0.0;
		roll = -PI * 0.5;
		lame_tweaks.cloud_hack = vec3( 2.6, 0.12, 0.22 );
		mouse_ctrl = 0.5;
		mm_offset = vec2( -0.07, 0.018 );
	}

	vec3 up = normalize( eye ); // horizontal views

	vec2 mm = ( iMouse.xy - iResolution.xy * 0.5 ) / ( min( iResolution.x, iResolution.y ) * 0.5 );

#ifndef SHADERTOY_STANDALONE
	if ( !key_state( KEY_SPACE ) ) mm = vec2( 0.0, 0.0 );
#endif

#ifdef NO_MOUSE_CTRL
	mm = vec2( 0.0, 0.0 );
#endif

	mm.x = -mm.x;
	mm = sign( mm ) * pow( abs( mm ), vec2( 0.9 ) );
	mm *= PI * tan_half_fovy * mouse_ctrl;
	mm += mm_offset;

//	camera_time = GLOBALTIME; // comment to always show the same time slice
	lame_tweaks.cloud_flow_time = ( cloud_flow_time_offset + camera_time ) * cloud_flow_time_scale;
	lame_tweaks.earth_rot_time = ( earth_rot_time_offset + camera_time ) * earth_rot_time_scale;

	return lookat( eye, center, up ) * z_rotation( roll ) * yup_spherical_offset( mm.y, mm.x );
}

#ifdef SHADERTOY_STANDALONE
void main()
#else
void mainImage( out vec4 fragColor, in vec2 fragCoord )
#endif
{
	vec2 resolution = iResolution.xy;
	vec2 pixel = fragCoord.xy;
	fragColor = vec4( 0., 0., 0., 1 );

#ifdef HD_BLACK_BANDS
	float aspect = ( 16.0 / 9.0 ); // the ratio we want
	if ( iResolution.x < aspect * iResolution.y )
	{
		resolution.y = resolution.x * ( 1.0 / aspect );
		pixel.y -= ( iResolution.y - resolution.y ) * 0.5;
		if ( abs( pixel.y * 2.0 - resolution.y ) > resolution.y ) return;
	}
	else
	{
		resolution.x = resolution.y * aspect;
		pixel.x -= ( iResolution.x - resolution.x ) * 0.5;
		if ( abs( pixel.x * 2.0 - resolution.x ) > resolution.x  ) return;
	}
#else
	float aspect = ( resolution.x / resolution.y );
#endif
	vec2 uv = ( pixel + vec2( 0.5, 0.5 ) ) / resolution.xy;

	float fade = 1.0;
	float tan_half_fovy = 0.06; // so we can see then sun
	float znear = 0.1;
	LameTweaks lame_tweaks;
	float exposure = 0.6;

#ifdef SHADERTOY_EXTRA_CAMERA
	mat4 camera;
	camera[0] = iCamera[0];
	camera[1] = iCamera[1];
	camera[2] = iCamera[2];
	camera[3] = iCamera[3];
	camera[3].xyz *= 1e+3 * atm_scale;
#else
	float fast_forward = ( key_state( KEY_RIGHT ) ? 5.0 : 1.0 );
	mat4 camera = get_earth_camera( tan_half_fovy, fade, GLOBALTIME * fast_forward, lame_tweaks, exposure );
#endif

	Ray view_ray = get_view_ray( ( uv - vec2( 0.5 ) ) * 2.0, znear, aspect, tan_half_fovy );

	view_ray.o = camera[3].xyz;
	view_ray.d = ( camera * vec4( view_ray.d, 0 ) ).xyz;

#ifdef SUPER_SAMPLE_HORIZON
	// do SS on edge pixels... still expensive
	float ss = vec3( 1 - exp( -abs( length( project_on_line1( earth_center, view_ray.o, view_ray.d ) - earth_center ) - earth_radius ) * 0.1 ) );
	fragColor = vec4( ss, ss, ss, 1.0 );
	if ( ss < 0.7 )
	{
		vec2 pmin = ( pixel + vec2( 0.0, 0.0 ) ) / resolution.xy;
		vec2 pmax = ( pixel + vec2( 1.0, 1.0 ) ) / resolution.xy;

		fragColor.rgb = vec3( 0 );
		for ( int i = 0; i < 2; i++ )
		{
			for ( int j = 0; j < 2; j++ )
			{
				vec2 uv2 = pmin + ( pmax - pmin )
					* vec2(
					0.5 + float( i ) / 2.0,
					0.5 + float( j ) / 2.0 );
				Ray subray = get_view_ray( ( uv2 - vec2( 0.5 ) ) * 2.0, znear, aspect, tan_half_fovy );
				subray.o = camera[3].xyz; // make sure all rays have same origin! we don't really care about havnig a znear here
				subray.d = ( camera * vec4( subray.d, 0 ) ).xyz;
				fragColor.rgb += earthShader( subray, camera, lame_tweaks, exposure );
			}
		}
		fragColor.rgb /= 4.0;
		return;
	}
#endif

	fragColor.rgb = earthShader( view_ray, camera, lame_tweaks, exposure ) * fade;
	return;
}
