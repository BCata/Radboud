function z = responsibility( X, mu, pi )

[N, D] = size(X);
K = length(pi);
z = zeros(N,K);

for n=1:N
    denom = 0;
    for j=1:K
        denom = denom + pi(j).*bernll(X(n,:), mu(j,:));
    end
    for k=1:K
       z(n, k) = (pi(k).*bernll(X(n,:), mu(k,:)))./denom; 
    end
end

end

