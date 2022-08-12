:- module(input, [zone/2, bl_landing_site/1, mars_zone/1, g/1]).

%%% The zone is 7000m wide and 3000m high.
zone(7000, 3000).

%%% The limit for the landing site of the shuttle must be at least
bl_landing_site(1000).

/* The number surfaceN of points used to draw the surface of Mars.
 * a couple of integers landX landY providing the coordinates of a ground point. 
 * By linking all the points together in a sequential fashion, you form the surface of Mars which is composed 
 * of several segments. 
 * For the first point, landX = 0 and for the last point, landX = 6999
 */
mars_zone([
     surface(0, 100),
     surface(1000, 500),
     surface(1500, 1500),
     surface(3000, 1000),
     surface(4000, 150),
     surface(5500, 150),
     surface(6999, 800)
]).

% %%% Start position of the lander and amount of carburants in litres
% lander(500, 2000, 4500).

%%% The game simulates a free fall without atmosphere. Gravity on Mars is 3.711 m/s².
%%% FIXME: try to fix the language grammar for float number 
%%% SOLVED: with better sintax for Prolog extension
g(3.711).