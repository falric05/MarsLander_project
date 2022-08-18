s1:-consult('C:/Users/User/Documents/Unibo/LA4AI/Mod1/Project/MarsLander_project/modules/mutil').
s2:-consult('C:/Users/User/Documents/Unibo/LA4AI/Mod1/Project/MarsLander_project/modules/mlander').
s3:-consult('C:/Users/User/Documents/Unibo/LA4AI/Mod1/Project/MarsLander_project/modules/minput').
s4:-consult('C:/Users/User/Documents/Unibo/LA4AI/Mod1/Project/MarsLander_project/modules/mcheck').
s5:-consult('C:/Users/User/Documents/Unibo/LA4AI/Mod1/Project/MarsLander_project/modules/mhelper').
% :-consult('C:/Users/User/Documents/Unibo/LA4AI/Mod1/Project/MarsLander_project/modules/mbezier').
% :-consult('C:/Users/User/Documents/Unibo/LA4AI/Mod1/Project/MarsLander_project/modules/testModule').
make_all:- s1, s2, s3, s4, s5, write("make done"), halt.