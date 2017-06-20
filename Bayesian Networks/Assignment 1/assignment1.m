addpath('..\FullBNT\BNT');

N = 12;             % number of variables

% define nodes (for readability)
Ag = 1; % age
R = 2;  % race
Se = 3; % sex (gender)
H = 4;  % hidden variable
Sp = 5; % sports
B = 6;  % bullied
U = 7;  % (feeling) unsecure at school
D = 8;  % depression
Al = 9; % alcohol consumption
F = 10; % fight
G = 11; % grades
W = 12; % weapon

% define connections
dag = zeros(N,N);   % matrix containing the connections (0=no connection)
dag(Ag,[Al W]) = 1;
dag(R,[B F H]) = 1;
dag(Se,[F G W]) = 1;
dag(H,G) = 1;
dag(B,[U D F]) = 1;
dag(U,[D W]) = 1;
dag(D,Al) = 1;
dag(Al,[G W]) = 1;
dag(F,G) = 1;
dag(G,W) = 1;

draw_graph(dag);
