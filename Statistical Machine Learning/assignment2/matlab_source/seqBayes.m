function [ mu ] = seqBayes( mut, sigmat, mup, sigmap, X )

N = size(X,1);
currMU = mup;
currSigma = sigmap;
mu = zeros(2,N)

for n = 1:N
    S = inv(inv(sigmat) + inv(currSigma));
    currMU = S*(inv(sigmat)*transpose(X(n,:)) + inv(currSigma)*currMU);
    currSigma = S;
    mu(:,n) = currMU'
end

end