N = 4;
X = [-0.5 0.2 0.3 -0.1]';
T = [0.5 -1 3 -2.5]';
K = zeros(N, N);
k = zeros(N,1);
theta = [1 1 1 1];
b = 1;

xnew = 0;

for i=1:N
    for j=1:N
        K(i,j) = kernel(X(i), X(j), theta);
    end
    k(i,1) = kernel(X(i), xnew, theta);
end

C = K + (1./b).*eye(N);
c = kernel(xnew, xnew, theta) + (1./b);
Cnew = zeros(N+1,N+1);
Cnew(1:N,1:N) = C;
Cnew(N+1, 1:N) = k';
Cnew(1:N, N+1) = k;
Cnew(N+1,N+1) = c;

m = k'/C*T
sigma = c - k'/C*k;
