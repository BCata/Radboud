function z = responsibility( X, mu, sigma, pi )

[N, D] = size(X);
K = length(pi);
z = zeros(N,K);

for n=1:N
    denom = 0;
    for j=1:K
        denom = denom + pi(j).*mvnpdf(X(n,:), mu(j,:), sigma(:,:,j));
    end
    for k=1:K
       z(n, k) = (pi(k).*mvnpdf(X(n,:), mu(k,:), sigma(:,:,k)))./denom; 
    end
end

end

