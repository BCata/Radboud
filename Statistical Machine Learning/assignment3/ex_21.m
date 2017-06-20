N = length(X);
mu1 = [0 0];
mu2 = [1 1];
sigma = 0.2.*eye(2);
phi = zeros(3,N);
phi(1,:) = ones(1,N);
phi(2,:) = mvnpdf(X,mu1,sigma);
phi(3,:) = mvnpdf(X,mu2,sigma);
t = C;
w = [0 ; 0 ; 0];

for i=1:10
    y = sigmoid((w'*phi)');
    dE = phi*(y-t);
    R = diag(y.*(ones(length(y),1)-y));
    H = phi*R*phi';
    w = w - inv(H)*dE
end