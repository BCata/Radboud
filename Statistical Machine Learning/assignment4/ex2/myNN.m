function [ OUT ] = myNN( IN, weights1, weights2 )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

%every row in IN represents one set of input
%IN is a matrix where number of columns represents number of inputs for NN
%and number of rows represents how many different sets of input we have

%weights1 and weights2 represents weights for first and second layer,
%respectively.

sz = size(IN);
N = sz(1,1);
OUT = zeros(N, 1);

for i=1:N
    in = [IN(i, :) 1];
    a1 = in*weights1;
    z1 = tanh(a1);
    OUT(i, :) = [z1 1]*weights2;
end

end

