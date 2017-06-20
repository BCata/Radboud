function [uML] = seqML( X )

N = size(X,1);
res = [0;0];
uML = zeros(2,N);

for n = 1:N
   res = res + (1./n)*(X(n,:)'-res);
   uML(:,n) = res;
end

