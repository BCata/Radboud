corr = zeros(K, 1);
for k=1:K
   corr(k,:) = sigma(1,2,k)./(sqrt(sigma(1,1,k).*sigma(2,2,k))); 
end