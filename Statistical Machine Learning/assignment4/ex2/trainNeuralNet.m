function [ w1, w2 ] = trainNeuralNet( IN, T, weights1, weights2 )
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here

%numOfIn = 2;
%numOfHidden = 8;
%numOfOut = 1;

%weights1 = rand([numOfIn+1, numOfHidden])-0.5;
%weights2 = rand([numOfHidden+1, numOfOut])-0.5;

sz = size(IN);
N = sz(1,1);
l = 0.001;

perm = randperm(N);

for n=1:N
    i = perm(1,n);
    
    in = [IN(i, :) 1];
    a1 = in*weights1;
    z1 = tanh(a1);
    out = [z1 1]*weights2;

    delta2 = out - T(i, :);
    delta1 = [(1 - z1.^2) 1].*(delta2.*weights2)';

    grad2 = delta2.*[z1 1]';
    grad1 = in'*delta1(:, 1:length(delta1)-1);

    weights1 = weights1 - l.*grad1;
    weights2 = weights2 - l.*grad2;
end
w1 = weights1;
w2 = weights2;

end

