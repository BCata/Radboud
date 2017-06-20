N = 101;
X = linspace(-1,1,N);
K = zeros(N, N);
theta = [1 1 1 1];

for i=1:N
    for j=1:N
        K(i,j) = kernel(X(i), X(j), theta);
    end
end

hold on;
mu = zeros(N,1);

Y = mvnrnd(mu, K);
p = plot(X,Y);
p.LineWidth = 2;

Y = mvnrnd(z, K);
p = plot(X,Y);
p.LineWidth = 2;

Y = mvnrnd(z, K);
p = plot(X,Y);
p.LineWidth = 2;

Y = mvnrnd(z, K);
p = plot(X,Y);
p.LineWidth = 2;

Y = mvnrnd(z, K);
p = plot(X,Y);
p.LineWidth = 2;