function res = evalLogLH( X, mu, sigma, pi )

K = length(pi);
res = 0;
[N, D] = size(X);


for n=1:N
    temp = 0;
    for k=1:K
        temp = temp + pi(k).*mvnpdf(X(n,:), mu(k,:), sigma(:,:,k));
    end
    res = res + log(temp);
end


%{
for i=1:K
    res = res + pi(i).*sum(mvnpdf(X, mu(i,:), sigma(:,:,i)));
end
%}

end

