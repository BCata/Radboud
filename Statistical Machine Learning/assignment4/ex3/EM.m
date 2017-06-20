X = load('a011_mixdata.txt', '-ASCII');
[N, D] = size(X);

K = 4;
pi = [1./K 1./K 1./K 1./K]';
meanx1 = mean(X(:,1));
meanx2 = mean(X(:,2));
meanx3 = mean(X(:,3));
meanx4 = mean(X(:,4));
means = [meanx1 meanx2 meanx3 meanx4];

%columns represents variable from x1 to x4
%rows represents K different classes
mu = zeros(K,D);
%sigma(:,:,k) represents covariance matrix for k-th class
sigma = zeros(D, D, K);
for i=1:K
   mu(i,:) =  means + rand(1,4).*2-1;
   sigma(:,:,i) = diag(4.*rand(1,D)+2);
end

initLH = evalLogLH(X, mu, sigma, pi);
fprintf('Init likelihood = %f\n', initLH);

maxIter = 100;
for i=1:maxIter
    %Expectation step
   resp = responsibility(X,mu,sigma,pi);                                                                                    
   
   %Maximization step
   Nk = sum(resp);
   for k=1:K
      mu(k,:) = (resp(:,k)'*X)./Nk(k);
   end
   
   for k=1:K
      temp = zeros(D,D);
      for n=1:N
          temp = temp + resp(n,k).*(X(n,:)-mu(k,:))'*(X(n,:)-mu(k,:));
      end
      sigma(:,:,k) = temp./Nk(k);
   end
   
   pi = Nk./N;
   
   LH = evalLogLH(X, mu, sigma, pi);
   fprintf('Step %d: likelihood = %f\n', i, LH);
end

