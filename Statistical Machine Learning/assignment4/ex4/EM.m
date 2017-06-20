[N, D] = size(X);

K = 3;
pi = zeros(1, K);
for i=1:K
   pi(1,i) = 1./K; 
end
mu = zeros(K,D);
for i=1:K
   mu(i,:) =  rand(1,D)*0.5+0.25;
end

initLH = evalLogLH(X, mu, pi);
fprintf('Init likelihood = %f\n', initLH);

maxIter = 40;
for i=1:maxIter
    %Expectation step
   resp = responsibility(X,mu,pi);                                                                                    
   
   %Maximization step
   Nk = sum(resp);
   for k=1:K
      mu(k,:) = (resp(:,k)'*X)./Nk(k);
   end
   
   pi = Nk./N;
   
   LH = evalLogLH(X, mu, pi);
   fprintf('Step %d: likelihood = %f\n', i, LH);
   
end

mudraw = mu.*255;
d = sqrt(D);
for k=1:K
    image(reshape(mudraw(k,:),d,d));
    colormap(gray(255));
    pause;
end

