function [ mun ] = seqMAP( mu0, sigma0, sigmat, X )

N = size(X,1);
muml = [0;0];
mun = zeros(2,N);

for n = 1:N
    muml = muml + (1./n)*(X(n,:)'-muml);
    mun(:,n) = ((sigmat*sigmat)/(n*(sigma0*sigma0) + sigmat*sigmat))*mu0 +...
        ((n*(sigma0*sigma0))/(n*(sigma0*sigma0) + sigmat*sigmat))*muml;
end

end