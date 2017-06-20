A = [11.85 2.2 0.5 4.0];
B = [11.95 3.1 0.0 1.0];
C = [12.00 2.5 0.0 2.0];
D = [12.00 3.0 1.0 6.3];

M = [A;B;C;D];

probs = zeros(4,K);
for k=1:K
    probs(:,k) = pi(k).*mvnpdf(M, mu(k,:), sigma(:,:,k));
end