probs = zeros(N,K);

for n=1:N
    for k=1:K
        probs(n,k) = log(pi(k).*bernll(X(n,:), mu(k,:)));
    end
end

[M, I] = max(probs');
realI = zeros(1, N);
for i=1:N
   if I(i) == 1
       realI(i) = 3;
   elseif I(i) == 2
       realI(i) = 2;
   elseif I(i) == 3
       realI(i) = 4;
   end
end

image(reshape(X(777,:)+1, d, d));
colormap(autumn(2));
pause;

mudraw = mu.*255;
d = sqrt(D);
for k=1:K
    image(reshape(mudraw(k,:),d,d));
    colormap(gray(255));
    pause;
end

count = 0;
r = realI'-Z;
for i=1:N
   if r(i) == 0
      count = count + 1; 
   end
end

perc = count./N