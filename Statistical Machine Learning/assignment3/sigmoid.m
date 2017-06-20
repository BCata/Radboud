function [ res ] = sigmoid( X )
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here
N = length(X);
res = zeros(N,1);

for i=1:N
    res(i,1) = 1 ./ (1+exp(-X(i,1)));
end

end

