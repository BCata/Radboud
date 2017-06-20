function res = evalLogLH( X, mu, pi )

K = length(pi);
res = 0;
[N, D] = size(X);


for n=1:N
    temp = 0;
   for k=1:K
       b = bernll(X(n,:), mu(k,:));
       temp = temp + pi(k).*b;
   end
   res = res + log(temp);
end


%{
for i=1:K
    res = res + pi(i).*sum(mvnpdf(X, mu(i,:), sigma(:,:,i)));
end
%}

end

