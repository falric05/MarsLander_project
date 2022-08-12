:- module(input, [zone/2, bl_landing_site/1, mars_zone/1, g/1]).

%%% The zone is 7000m wide and 3000m high.
zone(7000, 3000).

%%% The limit for the landing site of the shuttle must be at least
bl_landing_site(1000).

%%% The game simulates a free fall without atmosphere. Gravity on Mars is 3.711 m/s².
g(3.711).

%%% Mars surface segments anchor points
mars_zone([
	surface(0, 1500),
	surface(1000, 2000),
	surface(2000, 500),
	surface(3500, 500),
	surface(5000, 1500),
	surface(6999, 1000)
]).