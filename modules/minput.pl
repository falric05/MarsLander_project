:- module(input, [zone/2, bl_landing_site/1, mars_zone/1, g/1]).

%%% The zone is 7000m wide and 3000m high.
zone(7000, 3000).

%%% The limit for the landing site of the shuttle must be at least
bl_landing_site(1000).

%%% The game simulates a free fall without atmosphere. Gravity on Mars is 3.711 m/s².
g(3.711).

%%% Mars surface segments anchor points
mars_zone([
	surface(0, 100),
	surface(1000, 500),
	surface(1500, 1500),
	surface(3000, 1000),
	surface(4000, 150),
	surface(5500, 150),
	surface(6999, 800)
]).